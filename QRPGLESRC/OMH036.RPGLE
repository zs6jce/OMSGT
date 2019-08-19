     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Program ....: OMH036                                          *
      * Function ...: Create mail message from template               *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC - Helpdesk                               *
      *               P#INC# - Incident                               *
      *               P#CUSC - Customer                               *
      *               P#SUL# - Support level                          *
      *               P#STAT - Return status                          *
      *                                                               *
      * This program sends a mail message to a customer contact       *
      * using a mail template.                                        *
      *                                                               *
      * The program sets up an array used to communicate with an      *
      * SMTP server. After the array is set up, the program exits     *
      * without closing. After this is done, the communication        *
      * program is started which queries this program for each        *
      * array element (line of data) to send to an SMTP server.       *
      *                                                               *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * File specifications                                           *
      * ------------------------------------------------------------- *
      *                                                               *
      * OMMTI       - Mail template input file  (override to member)  *
      * OMMTO       - Mail template output file (override to member)  *
      * OMH036W3    - Work file with data                             *
      * OMINC       - Incidents                                       *
      * OMTIH       - Incident headers                                *
      * OMTID       - Incident details                                *
      * OMCUS       - Customers                                       *
      * OMCCO       - Customer contacts                               *
      * OMCCA       - Customer calls                                  *
      *                                                               *
      * ------------------------------------------------------------- *
     FOMMTI     IF   E             DISK
     F                                     RENAME(OMMTI:OMMTIR)
     F                                     RECNO(MTIRRN)
     FOMMTO     O    E             DISK
     F                                     RENAME(OMMTO:OMMTOR)
     FOMINCL1   IF   E           K DISK
     FOMCUSL1   IF   E           K DISK
     FOMCCOL1   IF   E           K DISK
     FOMCCAL1   IF   E           K DISK
     FOMTIHL1   IF   E           K DISK
     FOMTIDL1   IF   E           K DISK
     FOMSTRL1   IF   E           K DISK
     FOMSTAL1   IF   E           K DISK
     FOMHQLL1   IF   E           K DISK
     FOMQLQL1   IF   E           K DISK
     FOMQLAL1   IF   E           K DISK
     FOMHQUL1   IF   E           K DISK
     FOMANSL1   IF   E           K DISK
     FOMSREL1   IF   E           K DISK
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Extension specifications                                      *
      * ------------------------------------------------------------- *
      *                                                               *
      * STK   - The stack                                             *
      * BB    - Relative record numbers of begin blocks               *
      * EB    - Relative record numbers of end   blocks               *
      * CMD   - The commands allowed in a mail template               *
      *                                                               *
      * FOO   - The footer lines                                      *
      *                                                               *
      * OL    - The output line                                       *
      * IL    - The input line                                        *
      *                                                               *
      * #S    - The fields we can use for substition                  *
      * #L    - The length of the fields in the buffer                *
      * #B    - The starting position in the buffer                   *
      *                                                               *
      * ------------------------------------------------------------- *
     D STK             S              5  0 DIM(100)
     D BB              S              5  0 DIM(100)
     D EB              S              5  0 DIM(100)
     D CMD             S             50    DIM(14) CTDATA PERRCD(1)
      *
     D FOO             S             80    DIM(100)
      *
     D OL              S              1    DIM(80)
     D IL              S              1    DIM(80)
      *
     D #S              S              7    DIM(175) CTDATA PERRCD(1)
     D #L              S              2  0 DIM(175) CTDATA PERRCD(1)
     D #B              S              4  0 DIM(175) CTDATA PERRCD(1)
      *COPY QCPYSRC,OMSREFREN
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Input specifications                                          *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
      *
      *          ------------------------------------------
      *          W3 file
      *          ------------------------------------------
     D P#W3          E DS                  EXTNAME(OMH036W3)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PARM           *PARMS
      *
      *          ------------------------------------------
      *          Source data parsing
      *          ------------------------------------------
     D                 DS
     D SRCDTA                        80
     D  TS0150                       50    OVERLAY(SRCDTA)
     D   TS0105                       5    OVERLAY(TS0150)
     D    TS0101                      1    OVERLAY(TS0105)
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
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#PARM          C                   CONST(5)
      * Prototype for OMH036
     D OMH036          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#SUL#_                             LIKE(P#SUL#)
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
      * *ENTRY Interface for Main Procedure
     D OMH036          PI
     D P#HEDC                         5
     D P#INC#                        10
     D P#CUSC                        10
     D P#SUL#                         1
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D k#qls#          S              5P 0
     D k#que#          S              5P 0
     D BF              S              5  0
     D BLANKS          S             13
     D BX              S              5  0
     D CP              S              5  0
     D DO_X            S              7  0
     D DO_X10          S              7  0
     D DO_X11          S              7  0
     D DO_X12          S              7  0
     D DO_X2           S              7  0
     D DO_X3           S              7  0
     D DO_X4           S              7  0
     D DO_X5           S              7  0
     D DO_X6           S              7  0
     D DO_X7           S              7  0
     D DO_X8           S              7  0
     D DO_X9           S              7  0
     D EL              S              5  0
     D FF              S              5  0
     D FI              S              5  0
     D I#FOMO          S                   LIKE(INDI)
     D I#ITER          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
     D IY              S              5  0
     D K#HEDC          S                   LIKE(HEDC)
     D LF              S              5  0
     D LX              S              5  0
     D MTIRRN          S              5  0
     D N               S              5  0
     D NB              S              5  0
     D P#FILE          S             57
     D P#MSTA          S             40
     D S#IX            S              5  0
     D SC              S              5  0
     D SP              S              5  0
     D ST              S              5  0
     D W#BODY          S              5  0
     D W#CMD#          S              5  0
     D W#FLDC          S              7
     D W#OL            S             80
     D W#RRN#          S              5  0
     D W#WRK           S            256
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Main line                                                   *
      * ------------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Close if no paramters passed.
      *          ------------------------------------------
     C                   IF        D#PARM <> C#PARM                             B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   EXSR      SRVERW
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   RETURN
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRVERW - Processing                                           *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Check the option
      *          ------------------------------------------
     C                   EXSR      SRINIT
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRINIT - Create mail message from template                    *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRINIT        BEGSR
      *
      *          ------------------------------------------
      *          Inits
      *          ------------------------------------------
     C                   EVAL      SP = 101                                     Stackpointer
      *
     C                   EVAL      FI = *ZEROS                                  Footer index
     C                   MOVEA     *ALLX'00'     FOO                            Footer array
     C                   EVAL      I#FOMO = *OFF                                Footer mode
      *
      *          ------------------------------------------
      *          Position to the body.
      *          ------------------------------------------
     C                   IF        W#BODY <> *ZEROS                             B01
     C     W#BODY        CHAIN     OMMTI
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get the database
      *          ------------------------------------------
     C                   IF        P#INC# <> *BLANKS                            B01
     C     $NC1K1        CHAIN     OMINCL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *
     C     $CA1K1        CHAIN     OMCCAL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
     C     $US1K1        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
     C     $CO1K1        CHAIN     OMCCOL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          No incident
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   IF        P#CUSC <> *BLANKS                            B02
     C     P#CUSC        CHAIN     OMCUSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Read the first source line
      *          ------------------------------------------
     C                   READ      OMMTI
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Ignore remarks
      *          ------------------------------------------
     C                   IF        TS0101 <> '#'                                B02
      *
      *          ------------------------------------------
      *          Check if this is a commend.
      *          ------------------------------------------
     C                   EVAL      IY = 1
     C                   EVAL      IY = %LOOKUP(TS0150:CMD:IY)
     C                   IF        IY > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IY = 1
     C                   ENDIF
      *
      *          ------------------------------------------
      *          It is, do the correct routine.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      W#CMD# = IY
      *
      *          ------------------------------------------
      *          Is this an iteration?
      *          ------------------------------------------
     C                   IF        W#CMD# = 3                                   B04
     C                   EXSR      SRCM03
     C                   EVAL      *IN99 = *OFF
     C                   ITER
      *
      *          ------------------------------------------
      *          Other commands?
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   SELECT
     C                   WHEN      W#CMD# = 1
     C                   EXSR      SRCM01
     C                   WHEN      W#CMD# = 2
     C                   EXSR      SRCM02
     C                   WHEN      W#CMD# = 4
     C                   EXSR      SRCM04
     C                   WHEN      W#CMD# = 5
     C                   EXSR      SRCM05
     C                   WHEN      W#CMD# = 6
     C                   EXSR      SRCM06
     C                   WHEN      W#CMD# = 7
     C                   EXSR      SRCM07
     C                   WHEN      W#CMD# = 8
     C                   EXSR      SRCM08
     C                   WHEN      W#CMD# = 9
     C                   EXSR      SRCM09
     C                   WHEN      W#CMD# = 10
     C                   EXSR      SRCM10
     C                   WHEN      W#CMD# = 11
     C                   EXSR      SRCM11
     C                   WHEN      W#CMD# = 12
     C                   EXSR      SRCM12
     C                   WHEN      W#CMD# = 13
     C                   EXSR      SRCM13
     C                   WHEN      W#CMD# = 14
     C                   EXSR      SRCM14
     C                   ENDSL
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          It is not a command. Process the line.
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EXSR      SRPROL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C                   ENDIF                                                  E02
     C                   READ      OMMTI
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Clear the iteration flag
      *          ------------------------------------------
     C                   EVAL      I#ITER = *OFF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM01 - Command 1. <read all text headers>                   *
      *                                                               *
      * This routine reads all text headers. When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * Texts with support level > 0 are not included.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM01        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X = 1 TO 1                                B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first text header
      *          ------------------------------------------
     C     $IH1K1        SETLL     OMTIHL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C                   DOU       IHSUL# <= P#SUL#
     C     $IH1K1        READE     OMTIHL1
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
      *          ------------------------------------------
      *          If this is end of file, then
      *          set the pointer in the template to the matching
      *          iteration record, clear the stack and endsr.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM02 - Command 2. <read all text details>                   *
      *                                                               *
      * This routine reads all text details. When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM02        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X2 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first text detail. It is
      *          wise to have read a text header first.
      *          ------------------------------------------
     C     $ID1K1        SETLL     OMTIDL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     $ID1K1        READE     OMTIDL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM03 - Command 3. <iterate>                                 *
      *                                                               *
      * This routine checks the block currently on the stack. The     *
      * pointer is set to the begin of the block.                     *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM03        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X3 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          Get the record from the stack
      *          ------------------------------------------
     C                   EVAL      I#ITER = *ON
     C                   EVAL      MTIRRN = STK(SP)
     C     MTIRRN        CHAIN     OMMTI
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM04 - Command 4. <read all status routes>                  *
      *                                                               *
      * This routine reads all status routes When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM04        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X4 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first status route
      *          ------------------------------------------
     C     P#HEDC        SETLL     OMSTRL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     P#HEDC        READE     OMSTRL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file, then
      *          set the pointer in the template to the matching
      *          iteration record, clear the stack and endsr.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM05 - Command 5. <read all status route entries>           *
      *                                                               *
      * This routine reads all route entries When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM05        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X5 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first route entry. It is
      *          wise to have read a route header first.
      *          ------------------------------------------
     C     $RE1K1        SETLL     OMSREL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     $RE1K1        READE     OMSREL1
     C                   EVAL      *IN99 = %EOF
     C     $TA1K1        CHAIN     OMSTAL1
     C                   EVAL      *IN98 = NOT %FOUND
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM06 - Command 6. <read all customers>                      *
      *                                                               *
      * This routine reads all customers.    When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM06        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X6 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first customer
      *          ------------------------------------------
     C     *LOVAL        SETLL     OMCUSL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C                   READ      OMCUSL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file, then
      *          set the pointer in the template to the matching
      *          iteration record, clear the stack and endsr.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM07 - Command 7. <read all customer contacts>              *
      *                                                               *
      * This routine reads all contacts.     When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM07        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X7 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first route entry. It is
      *          wise to have read a route header first.
      *          ------------------------------------------
     C     USCUSC        SETLL     OMCCOL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     USCUSC        READE     OMCCOL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM08 - Command 8. <read all question list questions>        *
      *                                                               *
      * This routine reads all quest. lists. When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM08        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X8 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first list.
      *          ------------------------------------------
     C     P#HEDC        SETLL     OMQLQL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     P#HEDC        READE     OMQLQL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the question list.
      *          ------------------------------------------
     C                   EVAL      K#HEDC = P#HEDC
     C                   MOVE      LQQLS#        K#QLS#
     C     $QL1K1        CHAIN     OMHQLL1
     C                   EVAL      *IN98 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Get the question
      *          ------------------------------------------
     C                   EVAL      K#HEDC = P#HEDC
     C                   MOVE      LQQUE#        K#QUE#
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN98 = NOT %FOUND
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM09 - Command 9. <read all question list answers>          *
      *                                                               *
      * This routine reads all QL answers.   When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM09        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X9 = 1 TO 1                               B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first list. Read a question
      *          list question first.
      *          ------------------------------------------
     C     $LA1K1        SETLL     OMQLAL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     $LA1K1        READE     OMQLAL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Get the question list.
      *          ------------------------------------------
     C                   EVAL      K#HEDC = P#HEDC
     C                   MOVE      LAQLS#        K#QLS#
     C     $QL1K1        CHAIN     OMHQLL1
     C                   EVAL      *IN98 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Get the question
      *          ------------------------------------------
     C                   EVAL      K#HEDC = P#HEDC
     C                   MOVE      LAQUE#        K#QUE#
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN98 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Get the answer
      *          ------------------------------------------
     C     $NS1K2        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM10 - Command 10 <read all questions>                      *
      *                                                               *
      * This routine reads all questions.    When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM10        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X10 = 1 TO 1                              B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first question
      *          ------------------------------------------
     C     P#HEDC        SETLL     OMHQUL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     P#HEDC        READE     OMHQUL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM11 - Command 11 <read all question answers>               *
      *                                                               *
      * This routine reads all answers.      When the iteration flag  *
      * is set then the next record is read. If the iteration flag    *
      * was not set then the first record is read.                    *
      *                                                               *
      * If no more records where found then the processing is         *
      * continued at the matching iteration statement.                *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM11        BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X11 = 1 TO 1                              B01
      *
      *          ------------------------------------------
      *          If the command is not yet on the stack.
      *          ------------------------------------------
     C                   IF        I#ITER <> *ON                                B02
     C                   SUB       1             SP
     C                   EVAL      STK(SP) = MTIRRN
      *
      *          ------------------------------------------
      *          Set pointer in the first route entry. It is
      *          wise to have read a question first.
      *          ------------------------------------------
     C     $NS1K1        SETLL     OMANSL1
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read a record exit if end of file
      *          ------------------------------------------
     C     $NS1K1        READE     OMANSL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          If this is end of file at the first read, then
      *          set the pointer of the template at the matching
      *          iteration record, clear the stack and continue.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EXSR      SRPOP
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM12 - Command 12 <header>                                  *
      *                                                               *
      * Do nothing.                                                   *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM12        BEGSR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM13 - Command 13 <body>                                    *
      *                                                               *
      * This routine stores the next relative record number. This     *
      * indicates the start of the body.                              *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM13        BEGSR
      *
      *          ------------------------------------------
      *          Store the start of the body
      *          ------------------------------------------
     C                   EVAL      W#BODY = MTIRRN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRCM14 - Command 14 <footer>                                  *
      *                                                               *
      * This routine sets the program in footer mode. In footer mode  *
      * the program is not writing the detail lines in the outfile    *
      * but instead in an array. This array is written to the         *
      * ouput file when the program is closed.                        *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRCM14        BEGSR
      *
      *          ------------------------------------------
      *          Start footer mode.
      *          ------------------------------------------
     C                   EVAL      I#FOMO = *ON
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRPOP  - POP the block from the stack                         *
      *                                                               *
      * This routine gets the block currently on the stack. It then   *
      * sets the pointer to the matching iterate statement and        *
      * removes the record from the stack.                            *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRPOP         BEGSR
      *
      *          ------------------------------------------
      *          DO so we can LEAVE
      *          ------------------------------------------
     C                   FOR       DO_X12 = 1 TO 1                              B01
      *
      *          ------------------------------------------
      *          Get the begin of the block
      *          ------------------------------------------
     C                   EVAL      IX = SP
     C                   EVAL      IX = %LOOKUP(STK(SP):BB:IX)
     C                   IF        IX > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IX = 1
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Position to the matching end of the block
      *          ------------------------------------------
     C                   EVAL      MTIRRN = EB(IX)
     C     MTIRRN        CHAIN     OMMTI
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Remove the block from the stack
      *          ------------------------------------------
     C                   ADD       1             SP
      *
      *          ------------------------------------------
      *          End of do
      *          ------------------------------------------
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRPROL - Process the line.                                    *
      *                                                               *
      * The line is placed in the next free element of the array.     *
      * Substitution variables are parsed and replaced with their     *
      * actual current values.                                        *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRPROL        BEGSR
      *
      *          ------------------------------------------
      *          First some inits
      *          ------------------------------------------
     C                   MOVEA     SRCDTA        IL(1)
     C                   EVAL      W#OL = *BLANKS
     C                   EVAL      OL = *BLANKS
     C                   EVAL      IX = 1
     C                   EVAL      CP = 1
      *
      *          ------------------------------------------
      *          Do forever
      *          ------------------------------------------
     C                   DOW       1 = 1                                        B01
      *
      *          ------------------------------------------
      *          Find the first &
      *          ------------------------------------------
     C                   EVAL      S#IX = IX
     C                   EVAL      IX = %SCAN( '&' : SRCDTA:IX )
      *
      *                    --------------------------------
      *                    If no more found then exit
      *                    --------------------------------
     C                   IF        IX = *ZERO                                   B02
     C                   EVAL      IX = S#IX
     C                   LEAVE
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Check if this is a substitutable 7 value
      *          ------------------------------------------
     C                   EVAL      N = 7
     C                   EVAL      W#FLDC = %SUBST(SRCDTA:IX)
     C                   EVAL      FF = 1
     C                   EVAL      FF = %LOOKUP(W#FLDC:#S:FF)
     C                   IF        FF > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      FF = 1
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Check if this is a substitutable 5 value
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      N = 5
     C                   EVAL      W#FLDC = %SUBST(SRCDTA:IX:N)
     C                   EVAL      FF = 1
     C                   EVAL      FF = %LOOKUP(W#FLDC:#S:FF)
     C                   IF        FF > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      FF = 1
     C                   ENDIF
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    If not found then iterate if there
      *                    are characters left in the line
      *                    --------------------------------
     C                   IF        *IN99 = *OFF                                 B02
     C                   ADD       1             IX
     C                   IF        IX >= 80                                     B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   ITER
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Ok, a value was found.
      *          IX now points to the start of the & value and
      *          CP points to the end of the previous & value.
      *          CP is 1 if this is the first & value, like this.
      *
      *      1)  Dear Mister &CONM of company &CUNM,
      *          ¦           ¦
      *          CP          IX
      *
      *      2)  Dear Mister &CONM of company &CUNM,
      *                           ¦           ¦
      *                           CP          IX
      *
      *          Everything between CP and IX-1 is text and must be
      *          concatenated between the two values
      *          ------------------------------------------
      *
      *                    --------------------------------
      *                    Extract the substring between CP and IX-1
      *                    and place it in W#OL
      *                    --------------------------------
     C                   IF        IX > CP                                      B02
     C     IX            SUB       CP            EL
     C                   EVAL      W#WRK = %SUBST(SRCDTA:CP:EL)
     C                   EVAL      W#OL = %TRIMR(W#OL) + W#WRK
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Count the number of spaces between IX and the
      *          first charachter before IX
      *          ------------------------------------------
     C                   EVAL      SC = IX
     C                   EVAL      ST = *ZEROS
     C                   SUB       1             SC
     C                   DOW       SC > *ZEROS                                  B02
     C                             AND IL(SC) = *BLANKS
     C                   SUB       1             SC
     C                   ADD       1             ST
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Retrieve the value from the W3 buffer and
      *          concatenate it to the command buffer
      *          ------------------------------------------
     C                   EVAL      W#WRK = %SUBST(P#W3:#B(FF):#L(FF))
     C     W#OL          CAT       W#WRK:ST      W#OL
      *
      *                    --------------------------------
      *                    The new CP is the old IX + N so it
      *                    points to the next start of text
      *                    --------------------------------
     C     N             ADD       IX            CP
      *
      *          ------------------------------------------
      *          Increment IX and iterate if not end of string
      *          ------------------------------------------
     C                   ADD       1             IX
     C                   IF        IX > 80                                      B02
     C                   LEAVE
     C                   ELSE                                                   X02
     C                   ITER
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Concatenate the leftover
      *          ------------------------------------------
     C                   IF        IX = 1                                       B01
     C                   EVAL      W#OL = SRCDTA
     C                   ELSE                                                   X01
     C     80            SUB       CP            EL
     C                   EVAL      W#WRK = %SUBST(SRCDTA:CP:EL)
     C                   EVAL      W#OL = %TRIMR(W#OL) + W#WRK
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Write the output line if not in footer mode
      *          Write the array if in footer mode
      *          ------------------------------------------
     C                   IF        I#FOMO <> *ON                                B01
     C                   EVAL      SRCDTA = W#OL
     C                   WRITE     OMMTOR
     C                   ELSE                                                   X01
     C                   ADD       1             FI
     C                   EVAL      FOO(FI) = SRCDTA
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SRTTLR - Exit processing                                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Close if not enough parameters
      *          ------------------------------------------
     C                   IF        D#PARM <> C#PARM                             B01
      *
      *          ------------------------------------------
      *          Write the footers if required
      *          ------------------------------------------
     C                   EVAL      FI = 1
     C                   DOW       FOO(FI) <> *ALLX'00'                         B02
     C                             AND FI <= 100
     C                   EVAL      SRCDTA = FOO(FI)
     C                   WRITE     OMMTOR
     C                   ADD       1             FI
     C                   ENDDO                                                  E02
      *
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *PSSR  - Error subroutine                                     *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C                   IF        I#PSSR <> *ON                                B01
     C                   EVAL      I#PSSR = *ON
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
      *          Error
      *          ------------------------------------------
     C                   IF        D#PARM = C#PARM                              B02
     C                   EVAL      P#STAT = C#TERM
     C                   ENDIF                                                  E02
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
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *INZSR - Program initialization                               *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Explicit numeric field definitions
      *          ------------------------------------------
     C                   EVAL      S#IX = *ZEROS
     C                   EVAL      N = *ZEROS
     C                   EVAL      SC = *ZEROS
     C                   EVAL      SP = *ZEROS
     C                   EVAL      ST = *ZEROS
     C                   EVAL      CP = *ZEROS
     C                   EVAL      IY = *ZEROS
     C                   EVAL      IX = *ZEROS
     C                   EVAL      EL = *ZEROS
     C                   EVAL      LX = *ZEROS
     C                   EVAL      BX = *ZEROS
     C                   EVAL      FF = *ZEROS
     C                   EVAL      BF = *ZEROS
     C                   EVAL      LF = *ZEROS
     C                   EVAL      FI = *ZEROS
     C                   EVAL      NB = *ZEROS
     C                   EVAL      W#CMD# = *ZEROS
     C                   EVAL      MTIRRN = *ZEROS
     C                   EVAL      W#RRN# = *ZEROS
     C                   EVAL      W#BODY = *ZEROS
      *
      *          ------------------------------------------
      *          Other Workfields
      *          ------------------------------------------
     C                   EVAL      W#WRK = *BLANKS
     C                   EVAL      W#FLDC = *BLANKS
     C                   EVAL      BLANKS = *BLANKS
     C                   EVAL      W#OL = *BLANKS
      *
      *          ------------------------------------------
      *          Indicators
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Key field definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter lists
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          OMHQLL1 - Question lists
      *          ------------------------------------------
     C     $QL1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#QLS#
      *
      *          ------------------------------------------
      *          OMQUEL1 - Question lists
      *          ------------------------------------------
     C     $QU1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#QUE#
      *
      *          ------------------------------------------
      *          OMINCL1 - Incident file
      *          ------------------------------------------
     C     $NC1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#INC#
      *
      *          ------------------------------------------
      *          OMANSL1 - Status
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    QUQUE#
      *
     C     $NS1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    LAQUE#
     C                   KFLD                    LAANS#
      *
      *          ------------------------------------------
      *          OMCCAL1 - Call track file
      *          ------------------------------------------
     C     $CA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    NCCTN#
      *
      *          ------------------------------------------
      *          OMCUSL1 - Customer file
      *          ------------------------------------------
     C     $US1K1        KLIST
     C                   KFLD                    CACUSC
      *
      *          ------------------------------------------
      *          OMCCOL1 - Contact file
      *          ------------------------------------------
     C     $CO1K1        KLIST
     C                   KFLD                    CACUSC
     C                   KFLD                    CACCTC
      *
      *          ------------------------------------------
      *          OMTIHL1 - Incident text file
      *          ------------------------------------------
     C     $IH1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    NCINC#
      *
      *          ------------------------------------------
      *          OMSTRL1 - Status route file
      *          ------------------------------------------
     C     $TR1K1        KLIST
     C                   KFLD                    P#HEDC
      *
      *          ------------------------------------------
      *          OMTIDL1 - Incident text detail
      *          ------------------------------------------
     C     $ID1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    IHINC#
     C                   KFLD                    IHITH#
      *
      *          ------------------------------------------
      *          OMSREL1 - Status route entries
      *          ------------------------------------------
     C     $RE1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    TRSTRC
      *
      *          ------------------------------------------
      *          OMSTAL1 - Status
      *          ------------------------------------------
     C     $TA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    RESTAT
      *
      *          ------------------------------------------
      *          OMQLAL1 - Question list answers
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    LQQLS#
     C                   KFLD                    LQQSQ#
     C                   KFLD                    LQQUE#
      *
      *          ------------------------------------------
      *          Parse the template
      *          ------------------------------------------
     C                   EVAL      NB = 101
     C                   READ      OMMTI
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      W#RRN# = MTIRRN
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Add an unclosed block
      *          ------------------------------------------
     C                   IF        SRCDTA = CMD(1)                              B02
     C                             OR SRCDTA = CMD(2)
     C                             OR SRCDTA = CMD(4)
     C                             OR SRCDTA = CMD(5)
     C                             OR SRCDTA = CMD(6)
     C                             OR SRCDTA = CMD(7)
     C                             OR SRCDTA = CMD(8)
     C                             OR SRCDTA = CMD(9)
     C                             OR SRCDTA = CMD(10)
     C                             OR SRCDTA = CMD(11)
     C                   SUB       1             NB
     C                   EVAL      BB(NB) = MTIRRN
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Add an iteration
      *          ------------------------------------------
     C                   IF        SRCDTA = CMD(3)                              B03
     C                   EVAL      IX = NB
     C                   EVAL      IX = %LOOKUP(*ZEROS:EB:IX)
     C                   IF        IX > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IX = 1
     C                   ENDIF
     C                   EVAL      EB(IX) = MTIRRN
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next source record
      *          ------------------------------------------
     C                   READ      OMMTI
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Set at BOF
      *          ------------------------------------------
     C     W#RRN#        CHAIN     OMMTI
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Compile time arrays                                           *
      * ------------------------------------------------------------- *
** If you add more commands then you have to adjust the array length !!
<read all text headers>                            - 1
<read all text details>                            - 2
<iterate>                                          - 3
<read all status routes>                           - 4
<read all status route entries>                    - 5
<read all customers>                               - 6
<read all customer contacts>                       - 7
<read all question list questions>                 - 8
<read all question list answers>                   - 9
<read all questions>                               - 10
<read all question answers>                        - 11
<header>                                           - 12
<body>                                             - 13
<footer>                                           - 14
**
&INHEDC       Helpdesk
&ININCN       Incident
&INCTNN       Call track number
&INAINN       Accumulated incident number
&INIIDT       Incident income date
&INIIDC       Incident income date Century/Year
&INIIDY       Incident income date Year
&INIIDM       Incident income date Month
&INIIDD       Incident income date Day
&INIITM       Incident income time
&INIITH       Incident income time Hours
&INIIMM       Incident income time Minutes
&INIITS       Incident income time Seconds
&INICDT       Incident compl. date
&INICDC       Incident compl. date Century/Year
&INICDY       Incident compl. date Year
&INICDM       Incident compl. date Month
&INICDD       Incident compl. date Day
&INICTM       Incident compl. time
&INICTH       Incident compl. time Hours
&INICMM       Incident compl. time Minutes
&INICTS       Incident compl. time Seconds
&INSTRC       Status route code
&INRESN       Route entry sequence
&INSULN       Support level
&INSTAT       Status
&INSRDT       Status route date
&INSRDC       Status route date Century/Year
&INSRDY       Status route date Year
&INSRDM       Status route date Month
&INSRDD       Status route date Day
&INSRTM       Status route time
&INSRTM       Status route time Hours
&INSRMM       Status route time Minutes
&INSRTS       Status route time Seconds
&INSEV1       Technical severity
&INSEV2       Customer severity
&INSEV3       Marketing severity
&INPRIN       Priority
&INESCN       Escalation counter
&INIEDT       Last escalation date
&INIEDC       Last escalation date Century/Year
&INIEDY       Last escalation date Year
&INIEDM       Last escalation date Month
&INIEDD       Last escalation date Day
&INIETM       Last escalation time
&INIETH       Last escalation time Hours
&INIEMM       Last escalation time Minutes
&INIETS       Last escalation time Seconds
&INRUID       Registered by
&INAUID       Allocated by
&INITPC       Incident type
&INCFLN       Configuration list number
&INCFIC       Configuration item
&ININF1       User field 1
&ININF2       User field 2
&ININF3       User field 3
&INSHID       Short incident description
&CAHEDC       Helpdesk code
&CACTNN       Call track number
&CACUSC       Customer
&CACCTC       Contact
&CACIDT       Income date
&CACIDC       Income date Century/Year
&CACIDY       Income date Year
&CACIDM       Income date Month
&CACIDD       Income date Day
&CACITM       Income time
&CACITH       Income time Hours
&CACIMM       Income time Minutes
&CACITS       Income time Seconds
&CACCTM       Completion time
&CACCTH       Completion time Hours
&CACCMM       Completion time Minutes
&CACCTS       Completion time Seconds
&CAUSID       Registered by
&CACSRC       Call source
&CACAF1       User field 1
&CACAF2       User field 2
&CACAF3       User field 3
&COCUSC       Customer
&COCCTC       Contact
&COCNTI       Default contact
&COTITL       Title
&COCONM       Contact name
&COTELN       Telephone
&COEMAC       Email
&COCFLN       Configuration list
&COCOF1       User field 1
&COCOF2       User field 2
&COCOF3       User field 3
&CUCUSC       Customer code
&CUCUNM       Name
&CUADST       Street
&CUZIPC       Zip code
&CUADRE       Residence
&CUCOUD       Country
&CUCUF1       User field 1
&CUCUF2       User field 2
&CUCUF3       User field 3
&IHHEDC       Helpdesk code
&IHINCN       Incident number
&IHITHN       Incident text header seq.
&IHITXT       Incident text type
&IHHIDT       Income date
&IHHIDC       Income date Century/Year
&IHHIDY       Income date Year
&IHHIDM       Income date Month
&IHHIDD       Income date Day
&IHHITM       Income time
&IHHITH       Income time Hours
&IHHIMM       Income time Minutes
&IHHITS       Income time Seconds
&IHHSUP       Support level
&IHHUID       User i.d., or user class
&IHSITD       Short incident text desc.
&IDHEDC       Helpdesk code
&IDINCN       Incident number
&IDITHN       Incident text header seq.
&IDITXT       Incident text type
&IDSEQN       Sequence number
&IDINCD       Incident description
&TRHEDC       Helpdesk code
&TRSTRC       Status route code
&TRSTRI       Route indicator
&TRSTRD       Route description
&REHEDC       Helpdesk code
&RESTRC       Status route code
&RERESN       Route entry sequence
&RESULN       Support level
&RENSRC       Status route code
&RESTAT       Status
&TAHEDC       Helpdesk code
&TASTAT       Status
&TASTAD       Status description
&QLHEDC       Helpdesk code
&QLQLSN       Question list number
&QLDESC       Description
&LAHEDC       Helpdesk code
&LAQLSN       Question list number
&LAQSQN       Question sequence in Q.list
&LAQUEN       Question number
&LAANSN       Answer sequence
&LAGLSN       Goto list
&LAGQSN       Goto question sequence
&LASEV1       Severity 1
&LASEV2       Severity 2
&LASEV3       Severity 3
&LASTRC       Status route code
&LARESN       Route entry sequence
&LAPRIN       Priority
&LAITPC       Incident type code
&LAUSF1       User field 1
&LAUSF2       User field 2
&LAUSF3       User field 3
&QUHEDC       Helpdesk code
&QUQUEN       Question number
&QUQUEI       Question indicator
&QUQGRC       Question group code
&QUQUES       Question
&NSHEDC       Helpdesk code
&NSQUEN       Question number
&NSANSN       Answer sequence
&NSANSC       Answer
&LQHEDC       Helpdesk code
&LQQLSN       Question list number
&LQQSQN       Question sequence in Q.list
&LQQUEN       Question number
**   Field length
05          Helpdesk code
10          Incident number
10          Call track number
10          Accumulated incident number
07          Income date
03          Income date Century/Year
02          Income date Year
03          Income date Month
03          Income date Day
06          Income time
02          Income time Hours
02          Income time Minutes
02          Income time Seconds
07          Realized completion date
03          Realized completion date Century/Year
02          Realized completion date Year
02          Realized completion date Month
02          Realized completion date Day
06          Realized completion time
02          Realized completion time Hours
02          Realized completion time Minutes
02          Realized completion time Seconds
10          Status route code
03          Route entry sequence
01          Support level
05          Status
07          Status date
03          Status date Century/Year
02          Status date Year
02          Status date Month
02          Status date Day
06          Status time
02          Status time Hours
02          Status time Minutes
02          Status time Seconds
05          Severity 1
05          Severity 2
05          Severity 3
03          Priority
03          Escalation counter
07          Incident escalation date
03          Incident escalation date Century/Year
02          Incident escalation date Year
02          Incident escalation date Month
02          Incident escalation date Day
06          Incident escalation time
02          Incident escalation time Hours
02          Incident escalation time Minutes
02          Incident escalation time Seconds
10          Registered by user
10          Allocated by user
05          Incident type code
05          Configuration list number
12          Configuration item
10          User field 1
25          User field 2
15          User field 3
50          Short incident descript.
05          Helpdesk code
10          Call track number
10          Customer code
10          Customer contact code
07          Income date
03          Income date Century/Year
02          Income date Year
02          Income date Month
02          Income date Day
06          Income time
02          Income time Hours
02          Income time Minutes
02          Income time Seconds
06          Realized completion time
02          Realized completion time Hours
02          Realized completion time Minutes
02          Realized completion time Seconds
10          User i.d., or user class
01          Call source code
10          User field 1
25          User field 2
15          User field 3
10          Customer code
10          Customer contact code
01          Default contact indication
30          Title
50          Name
15          Telephone number
50          E-mail account
05          Configuration list number
10          User field 1
25          User field 2
15          User field 3
10          Customer code
50          Name
50          Address street
25          Zip code
50          Address residence
50          Country
10          User field 1
25          User field 2
15          User field 3
05          Helpdesk code
10          Incident number
03          Incident text header seq.
01          Incident text type
07          Income date
03          Income date Century/Year
02          Income date Year
02          Income date Month
02          Income date Day
06          Income time
02          Income time Hours
02          Income time Minutes
02          Income time Seconds
01          Support level
10          User i.d., or user class
50          Short incident text desc.
05          Helpdesk code
10          Incident number
03          Incident text header seq.
01          Incident text type
05          Sequence number
70          Incident description
05          Helpdesk code
10          Status route code
01          Route indicator
50          Route description
05          Helpdesk code
10          Status route code
03          Route entry sequence
01          Support level
10          Status route code
05          Status
05          Helpdesk code
05          Status
50          Status description
05          Helpdesk code
05          Question list number
50          Description
05          Helpdesk code
05          Question list number
03          Question sequence in Q.list
05          Question number
03          Answer sequence
05          Goto list
03          Goto question sequence
05          Severity 1
05          Severity 2
05          Severity 3
10          Status route code
03          Route entry sequence
03          Priority
05          Incident type code
10          User field 1
25          User field 2
15          User field 3
05          Helpdesk code
05          Question number
01          Question indicator
10          Question group code
56          Question
05          Helpdesk code
05          Question number
03          Answer sequence
56          Answer
05          Helpdesk code
05          Question list number
03          Question sequence in Q.list
05          Question number
**   Start positions in the buffer
0001        Helpdesk code
0006        Incident number
0016        Call track number
0026        Accumulated incident number
0036        Income date
0036        Income date Century/year
0037        Income date Year
0039        Income date Month
0041        Income date Day
0043        Income time
0043        Income time Hours
0045        Income time Minutes
0047        Income time Seconds
0049        Realized completion date
0049        Realized completion date Century/Year
0050        Realized completion date Year
0052        Realized completion date Month
0054        Realized completion date Day
0056        Realized completion time
0056        Realized completion time Hours
0058        Realized completion time Minutes
0060        Realized completion time Seconds
0062        Status route code
0072        Route entry sequence
0075        Support level
0076        Status
0081        Status date
0081        Status date Century/Year
0082        Status date Year
0084        Status date Month
0086        Status date Day
0088        Status time
0088        Status time Hours
0090        Status time Minutes
0092        Status time Seconds
0094        Severity 1
0099        Severity 2
0104        Severity 3
0109        Priority
0112        Escalation counter
0115        Incident escalation date
0115        Incident escalation date Century/Year
0116        Incident escalation date Year
0118        Incident escalation date Month
0120        Incident escalation date Day
0122        Incident escalation time
0122        Incident escalation time Hours
0124        Incident escalation time Minutes
0126        Incident escalation time Seconds
0128        Registered by user
0138        Allocated by user
0148        Incident type code
0153        Configuration list number
0158        Configuration item
0170        User field 1
0180        User field 2
0205        User field 3
0220        Short incident descript.
0270        Helpdesk code
0275        Call track number
0285        Customer code
0295        Customer contact code
0305        Income date
0305        Income date Century/Year
0306        Income date Year
0308        Income date Month
0310        Income date Day
0312        Income time
0312        Income time Hours
0314        Income time Minutes
0316        Income time Seconds
0318        Realized completion time
0318        Realized completion time Hours
0320        Realized completion time Minutes
0322        Realized completion time Seconds
0324        User i.d., or user class
0334        Call source code
0335        User field 1
0345        User field 2
0370        User field 3
0385        Customer code
0395        Customer contact code
0405        Default contact indication
0406        Title
0436        Name
0486        Telephone number
0501        E-mail account
0551        Configuration list number
0556        User field 1
0566        User field 2
0591        User field 3
0606        Customer code
0616        Name
0666        Address street
0716        Zip code
0741        Address residence
0791        Country
0841        User field 1
0851        User field 2
0876        User field 3
0891        Helpdesk code
0896        Incident number
0906        Incident text header seq.
0909        Incident text type
0910        Income date
0910        Income date Century/Year
0911        Income date Year
0913        Income date Month
0915        Income date Day
0917        Income time
0917        Income time Hours
0919        Income time Minutes
0921        Income time Seconds
0923        Support level
0924        User i.d., or user class
0934        Short incident text desc.
0984        Helpdesk code
0989        Incident number
0999        Incident text header seq.
1002        Incident text type
1003        Sequence number
1008        Incident description
1078        Helpdesk code
1083        Status route code
1093        Route indicator
1094        Route description
1144        Helpdesk code
1149        Status route code
1159        Route entry sequence
1162        Support level
1163        Status route code
1173        Status
1178        Helpdesk code
1183        Status
1188        Status description
1238        Helpdesk code
1243        Question list number
1248        Description
1298        Helpdesk code
1303        Question list number
1308        Question sequence in Q.list
1311        Question number
1316        Answer sequence
1319        Goto list
1324        Goto question sequence
1327        Severity 1
1332        Severity 2
1337        Severity 3
1342        Status route code
1352        Route entry sequence
1355        Priority
1358        Incident type code
1363        User field 1
1373        User field 2
1398        User field 3
1413        Helpdesk code
1418        Question number
1423        Question indicator
1424        Question group code
1434        Question
1690        Helpdesk code
1695        Question number
1700        Answer sequence
1703        Answer
1959        Helpdesk code
1964        Question list number
1969        Question sequence in Q.list
1972        Question number
