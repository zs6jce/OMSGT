     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMX962                                        *
      * Function ...: Generate HelpDesk numbers                     *
      * Author .....: Remain Software                               *
      * Parameters .: P#HEDC  Helpdesk code                         *
      *               P#OPTI  1=Incident number                     *
      *                       2=Call track number                   *
      *               P#CDE#  Code number                           *
      *               P#STAT  Status                                *
      *                                                             *
      * This program generates the next available incident or       *
      * call track number.                                          *
      * The program is still active after return. In order to       *
      * shut it down and close the files, call it again with no     *
      * parameters.                                                 *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used files                                                  *
      * ----------------------------------------------------------- *
      *                                                             *
      * OMHDDL1 - Helpdesk definition file (I/O).                   *
      * OMCCAL1 - Customer call (I).                                *
      * OMINCL1 - Incident file (I).                                *
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
     FOMHDDL1   UF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(HDDL1)
     FOMINCL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(INCL1)
     FOMCCAL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     USROPN
     F                                     INFDS(CCAL1)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Arrays                                                      *
      *                                                             *
      * E10   Contains number                                       *
      * ----------------------------------------------------------- *
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
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  E#FILE               201    208
     D  D#USID               254    263
      *
      *          ------------------------------------------
      *          File information data structures in case of error.
      *          ------------------------------------------
     D HDDL1           DS            57
     D INCL1           DS            57
     D CCAL1           DS            57
      *
      *          ------------------------------------------
      *          Number incrementer.
      *          ------------------------------------------
     D                 DS
     D  E10                    1     10
     D                                     DIM(10)
     D  D#ER10                 1     10
     D  D#ER01                11     11
     D  D#RR01                11     11  0
     D  D#ER02                12     13
     D  D#RR02                12     13  0
     D  D#ER03                14     16
     D  D#RR03                14     16  0
     D  D#ER04                17     20
     D  D#RR04                17     20  0
     D  D#ER05                21     25
     D  D#RR05                21     25  0
     D  D#ER06                26     31
     D  D#RR06                26     31  0
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY
      *
      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER
      *
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#NORM          C                   CONST('*NORM')
     D C#TERM          C                   CONST('*TERM')
      * Prototype for OMX967
     D OMX967          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#OPTI_                             LIKE(P#OPTI)
     D P#CDE#_                             LIKE(P#CDE#)
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
     D OMX967          PI
     D P#HEDC                         5
     D P#OPTI                         1
     D P#CDE#                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D w#uinc          S              2P 0
     D DO_X            S              7  0
     D ES              S              5  0
     D I#L1            S              1
     D I#L2            S              1
     D I#PSSR          S              1
     D IX              S              5  0
     D K#CDE#          S                   LIKE(CTN#)
     D P#FILE          S             57
     D P#MSTA          S             40
     D S#CNT           S              7  0
     D S#DIGN          S              2  0
     D S#DIGO          S              2  0
     D S#POSN          S              2  0
     D S#POSO          S              2  0
     D W#CERN          S              7  0
     D W#DIG           S              2  0
     D W#POS1          S              2  0
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
      *          Program parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Close program if called without parameters
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
     C                   EVAL      P#STAT = C#TERM
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Check which file to open.
      *          ------------------------------------------
     C                   IF        P#OPTI = '1'                                 B01
     C                   IF        I#L1 <> *ON                                  B02
     C                   OPEN      OMINCL1
     C                   EVAL      I#L1 = *ON
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
     C                   IF        P#OPTI = '2'                                 B02
     C                   IF        I#L2 <> *ON                                  B03
     C                   OPEN      OMCCAL1
     C                   EVAL      I#L2 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Read helpdesk code for update
      *          ------------------------------------------
     C     P#HEDC        CHAIN(E)  OMHDDL1
     C                   EVAL      *IN99 = %ERROR OR NOT %FOUND
      *
      *          ------------------------------------------
      *          Quit processing when not retrieved
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B01
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Put in 10-character array
      *          ------------------------------------------
     C                   IF        P#OPTI = '1'                                 B01
     C                   MOVE      DDCINC        D#ER10
     C                   MOVE      DDIINC        W#UINC
     C                   ELSE                                                   X01
     C                   IF        P#OPTI = '2'                                 B02
     C                   MOVE      DDCCTN        D#ER10
     C                   MOVE      DDICTN        W#UINC
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
     C                   EVAL      ES = 1
      *
      *          ------------------------------------------
      *          Get First Blank Character (if any)
      *          Position ES on first non-blank on the right
      *          ------------------------------------------
     C                   EVAL      ES = %LOOKUP(*BLANK:E10:ES)
     C                   IF        ES > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      ES = 1
     C                   ENDIF
     C                   IF        *IN99 <> *ON                                 B01
     C                   EVAL      S#POSO = 11
     C                   EVAL      ES = 10
     C                   ELSE                                                   X01
     C                   Z-ADD     ES            S#POSO
     C                   SUB       1             ES
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Count number of digits (on the right) (Max 4)
      *          Get Position of first Digit
      *          ------------------------------------------
     C                   EVAL      S#DIGO = 0
     C                   DOW       S#DIGO < 6                                   B01
     C                             AND ES > 0
     C                             AND E10(ES) >= '0'
     C                             AND E10(ES) <= '9'
     C                   Z-ADD     ES            S#POSO
     C                   ADD       1             S#DIGO
     C                   SUB       1             ES
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Get Current Fix number (numeric part)
      *          Do until Fix not already exist
      *          avoid loop
      *          ------------------------------------------
     C                   EVAL      *IN99 = *ON
     C                   EVAL      S#CNT = *ZERO
      *
     C                   DOW       *IN99 = *ON                                  B01
     C                             AND S#CNT < 1000000
      *
      *          ------------------------------------------
      *          Do depending on number of digits
      *          ------------------------------------------
     C                   EVAL      ES = S#POSO
     C                   SELECT                                                 B02
     C                   WHEN      S#DIGO = 1                                   X02
     C                   MOVEA     E10(ES)       D#ER01
     C                   EVAL      W#CERN = D#RR01
     C                   WHEN      S#DIGO = 2                                   X02
     C                   MOVEA     E10(ES)       D#ER02
     C                   EVAL      W#CERN = D#RR02
     C                   WHEN      S#DIGO = 3                                   X02
     C                   MOVEA     E10(ES)       D#ER03
     C                   EVAL      W#CERN = D#RR03
     C                   WHEN      S#DIGO = 4                                   X02
     C                   MOVEA     E10(ES)       D#ER04
     C                   EVAL      W#CERN = D#RR04
     C                   WHEN      S#DIGO = 5                                   X02
     C                   MOVEA     E10(ES)       D#ER05
     C                   EVAL      W#CERN = D#RR05
     C                   WHEN      S#DIGO = 6                                   X02
     C                   MOVEA     E10(ES)       D#ER06
     C                   EVAL      W#CERN = D#RR06
     C                   WHEN      S#DIGO = *ZERO                               X02
     C                   EVAL      W#CERN = *ZERO
     C     11            SUB       S#POSO        S#DIGO
     C                   IF        S#DIGO = *ZERO                               B03
     C                   EVAL      S#DIGO = 4
     C                   EVAL      S#POSO = 7
     C                   ELSE                                                   X03
     C                   IF        S#DIGO > 4                                   B04
     C                   EVAL      S#DIGO = 4
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Add Increment to this field
      *          ------------------------------------------
     C                   ADD       W#UINC        W#CERN
      *
      *          ------------------------------------------
      *          Determine number of digits new field
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      W#CERN >= 100000                             X02
     C                   EVAL      S#DIGN = 6
     C                   WHEN      W#CERN >= 10000                              X02
     C                   EVAL      S#DIGN = 5
     C                   WHEN      W#CERN >= 1000                               X02
     C                   EVAL      S#DIGN = 4
     C                   WHEN      W#CERN >= 100                                X02
     C                   EVAL      S#DIGN = 3
     C                   WHEN      W#CERN >= 10                                 X02
     C                   EVAL      S#DIGN = 2
     C                   OTHER                                                  X02
     C                   EVAL      S#DIGN = 1
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Determine new values for # Digits and Digit position
      *          ------------------------------------------
     C     S#DIGN        SUB       S#DIGO        W#DIG
     C                   EVAL      S#POSN = S#POSO
     C                   IF        W#DIG > *ZERO                                B02
     C     S#POSO        ADD       S#DIGO        W#POS1
     C                   FOR       DO_X = 1 TO W#DIG                            B03
     C                   IF        W#POS1 > 10                                  B04
     C                   SUB       1             S#POSN
     C                   ELSE                                                   X04
     C                   ADD       1             W#POS1
     C                   ENDIF                                                  E04
     C                   ENDFOR                                                 E03
     C                   ELSE                                                   X02
     C                   EVAL      S#DIGN = S#DIGO
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Determine new counted number
      *          ------------------------------------------
     C                   EVAL      ES = S#POSN
     C                   SELECT                                                 B02
     C                   WHEN      S#DIGN = 1                                   X02
     C                   Z-ADD     W#CERN        D#RR01
     C                   EVAL      E10(ES) = D#ER01
     C                   WHEN      S#DIGN = 2                                   X02
     C                   Z-ADD     W#CERN        D#RR02
     C                   MOVEA     D#ER02        E10(ES)
     C                   WHEN      S#DIGN = 3                                   X02
     C                   Z-ADD     W#CERN        D#RR03
     C                   MOVEA     D#ER03        E10(ES)
     C                   WHEN      S#DIGN = 4                                   X02
     C                   Z-ADD     W#CERN        D#RR04
     C                   MOVEA     D#ER04        E10(ES)
     C                   WHEN      S#DIGN = 5                                   X02
     C                   Z-ADD     W#CERN        D#RR05
     C                   MOVEA     D#ER05        E10(ES)
     C                   WHEN      S#DIGN = 6                                   X02
     C                   Z-ADD     W#CERN        D#RR06
     C                   MOVEA     D#ER06        E10(ES)
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Check if record already exists
      *          ------------------------------------------
     C                   EVAL      K#CDE# = D#ER10
     C                   IF        P#OPTI = '1'                                 B02
     C     $INCK1        SETLL     OMINCL1
     C                   EVAL      *IN99 = %EQUAL
     C                   ELSE                                                   X02
     C                   IF        P#OPTI = '2'                                 B03
     C     $INCK1        SETLL     OMCCAL1
     C                   EVAL      *IN99 = %EQUAL
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
      *
     C                   ADD       1             S#CNT
     C                   EVAL      S#POSO = S#POSN
     C                   EVAL      S#DIGO = S#DIGN
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Quit when free number was not found
      *          ------------------------------------------
     C     S#CNT         CABGE     1000000       VERW99
      *
      *          ------------------------------------------
      *          Update the Application record with new field
      *          ------------------------------------------
     C                   IF        P#OPTI = '1'                                 B01
     C                   MOVE      K#CDE#        DDCINC
     C                   ELSE                                                   X01
     C                   IF        P#OPTI = '2'                                 B02
     C                   MOVE      K#CDE#        DDCCTN
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
     C                   UPDATE    OMHDDR
      *
      *          ------------------------------------------
      *          Fill parameter field and set *NORM
      *          ------------------------------------------
     C                   EVAL      P#CDE# = K#CDE#
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C     VERW99        TAG
     C                   RETURN
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#DIG = *ZERO
     C                   EVAL      W#POS1 = *ZERO
     C                   EVAL      W#CERN = *ZERO
     C                   EVAL      IX = *ZERO
     C                   EVAL      ES = *ZERO
      *
      *          ------------------------------------------
      *          Savefield definition.
      *          ------------------------------------------
     C                   EVAL      S#DIGO = *ZERO
     C                   EVAL      S#DIGN = *ZERO
     C                   EVAL      S#POSO = *ZERO
     C                   EVAL      S#POSN = *ZERO
     C                   EVAL      S#CNT = *ZERO
      *
      *          ------------------------------------------
      *          Keylist fields
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keylist for file OMINCL1.
      *          ------------------------------------------
     C     $INCK1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    K#CDE#
      *
      *          ------------------------------------------
      *          Other API Parameter Definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
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
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INCL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMCCAL1 '                          X02
     C                   MOVEL     CCAL1         P#FILE
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
      *          Exit.
      *          ------------------------------------------
     C                   EVAL      D#PARM = 0
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRTTLR - Last record processing                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
