     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH047                                        *
      * Function ...: Rename Customer                               *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Change the rename in calling the API          *
      *                                                             *
      * Parameters .: P#CUSC     Customer to be renamed             *
      *               P#NAMD     Name                               *
      *               P#STAT     Status                             *
      *                                                             *
      * This program changes the Customer Code and Name             *
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
     FOMH047D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * FLD   Fields in error.                                       *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(10)
     D MSD             S              7    DIM(7) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D FLD             S              4    DIM(30)
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
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  E#FILE               201    208
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
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
      *          OMQCHGCU API
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGCUDS
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
     D C#SAME          C                   CONST('*SAME')
     D C#INTL          C                   CONST('V3R0M0  ')
     D C#NO            C                   CONST('*NO')
      * Prototype for OMH047
     D OMH047          PR
     D P#CUSC_                             LIKE(P#CUSC)
     D P#NAMD_                             LIKE(P#NAMD)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMH902C'
     D OMH902C         PR                  EXTPGM('OMH902C')
     D PGMQ_                               LIKE(PGMQ)
      *
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'OMQCHGCU'
     D OMQCHGCU        PR                  EXTPGM('OMQCHGCU')
     D K4INTL_                             LIKE(K4INTL)
     D K4ACTC_                             LIKE(K4ACTC)
     D K4BUF_                              LIKE(K4BUF)
     D K4PRC_                              LIKE(K4PRC)
     D K4ERR_                              LIKE(K4ERR)
     D K4FLD_                              LIKE(K4FLD)
      * Prototype for 'OMH901C'
     D OMH901C         PR                  EXTPGM('OMH901C')
     D K4MSID_                             LIKE(K4MSID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D K4MSGD_                             LIKE(K4MSGD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH902C (Clear message queue)
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
      *          Parameter list for OMH901C (send message)
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Retrieve authorisation;
      *          return if insufficient (status <> *NORM)
      * Prototype for 'OMQCHGCU'
     D OMQCHGCU_001    PR                  EXTPGM('OMQCHGCU')
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
     D OMH047          PI
     D P#CUSC                        10
     D P#NAMD                        50
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EROR01          S                   LIKE(INDI)
     D I#PSSR          S              1
     D LOOP01          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#CIM           S              4
     D P#FILE          S             57
     D P#HEDC          S                   LIKE(HEDC)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#SUL#          S                   LIKE(SUL#)
     D W#STAT          S                   LIKE(STAT)
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *
      *
      *
      *          ------------------------------------------
      *          Exit if no parameters
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Process the screen
      *          ------------------------------------------
     C                   EXSR      SRSC01
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SNDMSG - Send message                                        *
      *                                                              *
      * This routine displays an error wich occured during user      *
      * keyed data validation.                                       *
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
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRSC01 - Proces screen                                       *
      *                                                              *
      * This routine processes the 'Rename Customer' screen          *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRSC01        BEGSR
      *
      *          ------------------------------------------
      *          Initialize screen
      *          ------------------------------------------
     C                   EXSR      SRIN01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR01 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
     C                   EXFMT     OMH04701
      *
      *          ------------------------------------------
      *          Remove messages
      *          ------------------------------------------
     C                   EXSR      CLRMSG
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      *IN70 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN42 = *OFF
      *
      *          ------------------------------------------
      *          Process screen
      *          ------------------------------------------
     C                   SELECT                                                 B02
      *
     C                   WHEN      *INKC = *ON                                  X02
     C                   EVAL      P#STAT = C#EXIT
     C                   EXSR      SRTTLR
      *
     C                   WHEN      *INKE = *ON                                  X02
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
      *
     C                   WHEN      *INKL = *ON                                  X02
     C                   EVAL      P#STAT = C#CNCL
     C                   EXSR      SRTTLR
      *
     C                   OTHER                                                  X02
      *
     C                   EXSR      SRED01
      *
      *          ------------------------------------------
      *          Ask for confirmative Enter if user enters
      *          correct data
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B03
     C                   IF        *IN40 = *ON                                  B04
     C                   EVAL      MSGDTA = *BLANKS
     C                   EVAL      MSGID = 'RNM0005'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDSL                                                  E02
      *
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Do the processing.
      *          ------------------------------------------
     C                   EXSR      SRPR01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRIN01 - Initialize screen                                   *
      *                                                              *
      * This routine initializes the 'Rename Customer' screen        *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Set static fields
      *          ------------------------------------------
     C                   MOVE      P#CUSC        FMCUSC
     C                   MOVE      P#NAMD        FMNAMD
      *
      *          ------------------------------------------
      *          Refresh
      *          ------------------------------------------
     C                   EXSR      SRRF01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRED01 - Edit field values                                   *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Call the change object API to check the input
      *          ------------------------------------------
     C                   MOVE      '0'           K4UPD
     C                   MOVE      '0'           K4UPB
     C                   MOVE      '0'           K4EOP
     C                   MOVE      FMCUSC        K4CUSC
     C                   MOVE      FMNCUS        K4NCUS
      *
     C                   EVAL      K4INTL = C#INTL
     C                   EVAL      K4ACTC = '21'
     C                   CALLP     OMQCHGCU ( K4INTL : K4ACTC : K4BUF :
     C                             K4PRC : K4ERR : K4FLD )
      *
      *          ------------------------------------------
      *          Fill field error buffer from Parameter
      *          ------------------------------------------
     C                   MOVE      K4FLD         FLD
      *
      *          ------------------------------------------
      *          Seton the fields in error.
      *          ------------------------------------------
     C                   IF        K4MSTP = 'E'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( K4MSID : MSGF : MSGLIB :
     C                             K4MSGD )
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Call the change object API to check the input
      *          ------------------------------------------
     C                   MOVE      '0'           K4UPD
     C                   MOVE      '0'           K4UPB
     C                   MOVE      '0'           K4EOP
     C                   MOVE      FMCUSC        K4CUSC
     C                   MOVE      FMNNAM        K4NAMD
     C                   MOVEL(P)  C#SAME        K4ADST
     C                   MOVEL(P)  C#SAME        K4ZIPC
     C                   MOVEL(P)  C#SAME        K4ADRE
     C                   MOVEL(P)  C#SAME        K4COUD
     C                   MOVEL(P)  C#SAME        K4USF1
     C                   MOVEL(P)  C#SAME        K4USF2
     C                   MOVEL(P)  C#SAME        K4USFA
      *
     C                   EVAL      K4ACTC = '20'
     C                   CALLP     OMQCHGCU ( K4INTL : K4ACTC : K4BUF :
     C                             K4PRC : K4ERR : K4FLD )
      *
      *          ------------------------------------------
      *          Seton the fields in error.
      *          ------------------------------------------
     C                   IF        K4MSTP = 'E'
     C                   EVAL      *IN71 = *ON
     C                   EVAL      MSGF = 'OMHAPI'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH901C ( K4MSID : MSGF : MSGLIB :
     C                             K4MSGD )
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPR01 - Do the rename                                       *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Call the API
      *          ------------------------------------------
     C                   MOVE      '1'           K4UPD
     C                   MOVE      '1'           K4UPB
     C                   MOVE      '0'           K4EOP
     C                   MOVE      P#CUSC        K4CUSC
     C                   MOVE      FMNCUS        K4NCUS
      *
     C                   EVAL      K4ACTC = '21'
     C                   CALLP     OMQCHGCU ( K4INTL : K4ACTC : K4BUF :
     C                             K4PRC : K4ERR : K4FLD )
      *
      *          ------------------------------------------
      *          Call the API
      *          ------------------------------------------
     C                   MOVE      '1'           K4UPD
     C                   MOVE      '1'           K4UPB
     C                   MOVE      '0'           K4EOP
     C                   MOVE      FMNCUS        K4CUSC
     C                   MOVE      FMNNAM        K4NAMD
      *
     C                   EVAL      K4ACTC = '20'
     C                   CALLP     OMQCHGCU ( K4INTL : K4ACTC : K4BUF :
     C                             K4PRC : K4ERR : K4FLD )
      *
      *          ------------------------------------------
      *          End of subroutine.
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRF01 - Refresh                                             *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Turn off error indicators
      *          ------------------------------------------
     C                   EVAL      *IN(70) = *OFF
     C                   EVAL      *IN(71) = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   MOVE      FMCUSC        FMNCUS
     C                   MOVE      FMNAMD        FMNNAM
      *
      *          ------------------------------------------
      *          End of subroutine.
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
      *          Workfield definition
      *          ------------------------------------------
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
      *          Parameter list for OMH029 (Get info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P#HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P#STAT
     C                   PARM                    D#D7HD
     C                   PARM                    D#D8HD
     C                   PARM      C#NO          P#CIM
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
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $Y901C        PLIST
     C                   PARM                    K4MSID
     C                   PARM      'OMHAPI'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    K4MSGD
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
      *          Retrieve authorisation;
      *          return if insufficient (status <> *NORM)
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P#HEDC = *BLANKS
     C                   EVAL      P#STAT = *BLANKS
     C                   EVAL      P#CIM = C#NO
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD : P#CIM )
     C                   EVAL      W#STAT = P#STAT
     C                   IF        P#STAT <> C#NORM                             B01
     C                   EXSR      *PSSR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get message & severity code data
      *          ------------------------------------------
     C                   EXSR      SRGVAR
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
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 1                                  B01
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
      *          Close the API
      *          ------------------------------------------
     C                   CALLP     OMQCHGCU_001()
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
      *          Call the error handler
      *          ------------------------------------------
     C                   CALLP     OMH961 ( PGM : P#FILE )
     C                   ENDIF                                                  E01
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
      *          Exit.
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
**
