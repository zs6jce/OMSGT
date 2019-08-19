     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH007                                        *
      * Function ...: Asking questions                              *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Parameters .: P#HEDC  P#CTN#  P#INC#  P#QLS#  P#MMNT  I     *
      *               P#ACTN                                  I     *
      *               P#SEV1  P#SEV2  P#SEV3  P#STRC  P#RES#  O     *
      *               P#PRI#  P#ITPC  P#USF1  P#USF2  P#USF3  O     *
      *               P#STAT                                  O     *
      *                                                             *
      * This program is asking questions to the help desk employee  *
      * Notice: questionnumbers 0 is reserved                       *
      *                                                             *
      * Remark: This program is also used to test question lists.   *
      * To solve a nasty error, the action code *TEST was added as  *
      * a third allowed value. The result is that for action code   *
      * *TEST, the last record indicator *INLR is set on in SRTTLR. *
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
      * File specifications                                   ANSL1 *
      * ----------------------------------------------------------- *
     FOMANSL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ANSL1)
     FOMQLAL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QLAL1)
     FOMHQUL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HQUL1)
     FOMQAH     IF A E             DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(QAH)
     FOMHDDL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDDL1)
     FOMH007D   CF   E             WORKSTN
     F                                     INFSR(*PSSR)
     F                                     INFDS(INFO)
     F                                     SFILE(OMH00701:RRN01)
     F                                     SFILE(OMH00705:RRN02)
      * ------------------------------------------------------------ *
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * QUE   Used to store question numbers                         *
      * ANS   Used to store answers                                  *
      * QLS   Used to store question lists                           *
      * QSQ   Used to store question seq in question list            *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(10)
     D MSD             S              7    DIM(7) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
     D QUE             S              5  0 DIM(1000)
     D ANS             S              3  0 DIM(1000)
     D QLS             S              5  0 DIM(1000)
     D QSQ             S              3  0 DIM(1000)
      *
      *COPY QCPYSRC,OMSREFREN
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
     D HQUL1           DS            57
     D HDDL1           DS            57
     D ANSL1           DS            57
     D QLAL1           DS            57
     D QAH             DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Workfields
      *          ------------------------------------------
     D                 DS
     D IZ                             4  0
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
     D C#STOP          C                   CONST('*STOP')
     D C#NOTF          C                   CONST('*NOTF')
     D C#QUES          C                   CONST('*QUES')
     D C#HIST          C                   CONST('*HIST')
     D C#TEST          C                   CONST('*TEST')
     D C#SFL1          C                   CONST(7)
     D C#SFL2          C                   CONST(10)
      * Prototype for OMH007
     D OMH007          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CTN#_                             LIKE(P#CTN#)
     D P#INC#_                             LIKE(P#INC#)
     D P#QLS#_                             LIKE(P#QLS#)
     D P#ACTN_                             LIKE(P#ACTN)
     D P#SEV1_                             LIKE(P#SEV1)
     D P#SEV2_                             LIKE(P#SEV2)
     D P#SEV3_                             LIKE(P#SEV3)
     D P#STRC_                             LIKE(P#STRC)
     D P#RES#_                             LIKE(P#RES#)
     D P#PRI#_                             LIKE(P#PRI#)
     D P#ITPC_                             LIKE(P#ITPC)
     D P#USF1_                             LIKE(P#USF1)
     D P#USF2_                             LIKE(P#USF2)
     D P#USF3_                             LIKE(P#USF3)
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
      * Prototype for 'OMH008'
     D OMH008          PR                  EXTPGM('OMH008')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#QLS#_                             LIKE(P#QLS#)
     D P#QSQ#_                             LIKE(P#QSQ#)
     D P#QUE#_                             LIKE(P#QUE#)
     D P#ANS#_                             LIKE(P#ANS#)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMX306 (Format string)
      * Prototype for 'OMX306'
     D OMX306          PR                  EXTPGM('OMX306')
     D P#QUAD_                             LIKE(P#QUAD)
     D P#POS#_                             LIKE(P#POS#)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMHQUL1 Question file
      * Prototype for 'OMX929'
     D OMX929          PR                  EXTPGM('OMX929')
     D P#HEDC_                             LIKE(P#HEDC)
     D P#INC#_                             LIKE(P#INC#)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#MSID_                             LIKE(P#MSID)
     D P#MSGD_                             LIKE(P#MSGD)
     D P#MSGF_                             LIKE(P#MSGF)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH008 (Get question)
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
      * *ENTRY Interface for Main Procedure
     D OMH007          PI
     D P#HEDC                         5
     D P#CTN#                        10
     D P#INC#                        10
     D P#QLS#                         5  0
     D P#ACTN                         5
     D P#SEV1                         5
     D P#SEV2                         5
     D P#SEV3                         5
     D P#STRC                        10
     D P#RES#                         3  0
     D P#PRI#                         3  0
     D P#ITPC                         5
     D P#USF1                        10
     D P#USF2                        25
     D P#USF3                        15  0
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#ans#          S              3P 0
     D p#qsq#          S              3P 0
     D p#que#          S              5P 0
     D s#rr01          S              5P 0
     D s#rr02          S              5P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D I#PSSR          S              1
     D IX              S              5  0
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
     D P#FILE          S             57
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#MSGD          S                   LIKE(MSGDTA)
     D P#MSGF          S                   LIKE(MSGF)
     D P#MSID          S                   LIKE(MSID)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
     D P#POS#          S              2  0
     D P#QUAD          S                   LIKE(QUQUES)
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USID          S             10
     D RRN01           S              5  0
     D RRN02           S              5  0
     D SEL             S                   LIKE(INDI)
     D W#CRC1          S              5  0
     D W#CRC2          S              5  0
     D W#EOFI          S                   LIKE(INDI)
     D W#OPT1          S                   LIKE(FMOPT1)
     D W#OPT2          S                   LIKE(FMOPT2)
     D W#POSI          S                   LIKE(INDI)
     D W#QUES          S                   LIKE(QUQUES)
     D W#SAV#          S              5  0
     D W#STAT          S                   LIKE(STAT)
     D W#VALC          S              5  0
     D W1OPT1          S                   LIKE(FMOPT1)
     D W1OPT2          S                   LIKE(FMOPT2)
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
      *          Get the parameters
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
      *          If action indicates that program should
      *          ask questions, then goto screen 1. Else
      *          goto routine to write to history.
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      P#ACTN = C#QUES                              X01
     C                             OR P#ACTN = C#TEST                           X01
     C                   EXSR      SRSC01
     C                   WHEN      P#ACTN = C#HIST                              X01
     C                   EXSR      SRLAST
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Exit
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      *
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
      *
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
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC01 - Asking questions                                   *
      *                                                             *
      * This routine displays the first screen where the user can   *
      * answer asked questions                                      *   *
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
     C                   WRITE     OMH00703
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the appropriate screen
      *          ------------------------------------------
     C                   IF        P#STAT = C#STOP                              B03
     C                   EXFMT     OMH00709
     C                   ELSE                                                   X03
     C                   IF        P#STAT = C#NOTF                              B04
     C                   EXFMT     OMH00710
     C                   ELSE                                                   X04
     C                   IF        *IN90 = *OFF                                 B05
     C                   EXFMT     OMH00711
     C                   ELSE                                                   X05
     C                   EXFMT     OMH00702
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          End with F3 or F12
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = C#EXIT
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                             AND *IN40 = *OFF
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
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F12=Previous
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B04
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      IZ = 1
     C                   EVAL      IZ = %LOOKUP(00000:QUE:IZ)
     C                   IF        IZ > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IZ = 1
     C                   ENDIF
     C                   SUB       1             IZ
     C                   EVAL      P#QUE# = QUE(IZ)
     C                   EVAL      P#QSQ# = QSQ(IZ)
     C                   EVAL      P#QLS# = QLS(IZ)
     C                   EVAL      QUE(IZ) = 0
     C                   EVAL      QSQ(IZ) = 0
     C                   EVAL      QLS(IZ) = 0
     C                   IF        IZ = 1                                       B05
     C                   EVAL      *IN40 = *OFF
     C                   ENDIF                                                  E05
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F15=History
      *          ------------------------------------------
     C                   IF        *IN15 = *ON                                  B05
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          No question found, or questions completed
      *          ------------------------------------------
     C                   IF        P#STAT = C#STOP                              B06
     C                             OR P#STAT = C#NOTF                           B05
     C                   EXSR      SRLAST
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
      *          Reposition when required
      *          ------------------------------------------
     C                   IF        W#POSI = *ON                                 B03
     C                   EVAL      W#POSI = *OFF
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Same screen in case of blank Enter
      *          ------------------------------------------
     C                   IF        W#VALC = 0                                   B04
     C                             AND *INKL = *OFF                             B03
     C                             AND EROR01 = *OFF                            B03
      *
      *          ------------------------------------------
      *          Insert QCPYSRC/OMH007XX here if you want to
      *          exit the list on Enter)
      *          ------------------------------------------
     C                   EVAL      EROR01 = *ON
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
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN01 - Work with answer questions                         *   *
      *                                                             *
      * Init the answer questions                                   *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Set indicators that control the text in
      *          the screen header
      *          ------------------------------------------
     C                   SELECT                                                 B01
     C                   WHEN      P#ACTN = C#QUES                              X01
     C                   EVAL      *IN80 = *ON
     C                   WHEN      P#ACTN = C#TEST                              X01
     C                   EVAL      *IN80 = *OFF
     C                   ENDSL                                                  E01
      *
      *          ------------------------------------------
      *          Clear numeric array & counter
      *          ------------------------------------------
     C                   CLEAR                   QUE
     C                   CLEAR                   ANS
     C                   CLEAR                   IZ
      *
      *          ------------------------------------------
      *          Does help desk code exist?
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMHDDR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get first number and question, exit when
      *          a failure occurs
      *          ------------------------------------------
     C                   EVAL      P#QUE# = *ZEROS
     C                   CALLP     OMH008 ( P#HEDC : P#QLS# : P#QSQ# :
     C                             P#QUE# : P#ANS# : P#STAT )
     C                   IF        P#STAT = C#TERM                              B01
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill helpdesk & incident number
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        FMHEDC
     C                   MOVEL     P#CTN#        FMCTN#
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRD01 - Read next answer                                   *   *
      *                                                             *
      * Read next record                                            *   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRD01        BEGSR
      *
      *          ------------------------------------------
      *          Do it
      *          ------------------------------------------
     C     $LA1K1        READE     OMQLAL1
     C                   EVAL      *IN99 = %EOF
     C                   EVAL      W#EOFI = *IN99
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSF01 - Build answer questions subfile                     *   *
      *                                                             *
      * This routine builds the subfile                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSF01        BEGSR
      *
      *          ------------------------------------------
      *          Fill and format question
      *          ------------------------------------------
     C                   MOVEL     QUQUES        P#QUAD
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = C#NORM
     C                   MOVEL     P#QUAD        FMQUES
      *
      *          ------------------------------------------
      *          Read first allowed answer number
      *          ------------------------------------------
     C                   EVAL      P#ANS# = 0
     C     $LA1K2        SETLL     OMQLAL1
     C                   EXSR      SRRD01
      *
      *          ------------------------------------------
      *          Do while not end of answer file
      *          ------------------------------------------
     C                   EVAL      IX = *ZEROS
     C                   DOW       W#EOFI = *OFF                                B01
      *
      *          ------------------------------------------
      *          Get answer description
      *          ------------------------------------------
     C                   Z-ADD     LAANS#        P#ANS#
     C     $NS1K1        CHAIN     OMANSR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPT1
     C                   MOVEL     NSANSC        FMANSD
     C                   Z-ADD     LAANS#        FMANS#
     C                   ADD       1             RRN01
     C                   ADD       1             IX
     C                   WRITE     OMH00701
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
     C                   IF        W#EOFI = *ON                                 B01
     C                   EVAL      *IN92 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      *IN92 = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF01 - Work with answer refresh                               *
      *                                                             *
      * Actually this is only initialization                        *
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
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH00702
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
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED01 - Edit the answer questions screen                   *   *
      *                                                             *
      * This routine checks if the option fields in the subfile     *
      * where correctly filled. An answer can be viewed (5) or      *   *
      * selected                                                    *
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
     C                   CLEAR                   W1OPT1
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   IF        RRN01 > *ZERO                                B01
     C                   READC     OMH00701
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
     C                   MOVEL     FMOPT1        W#OPT1
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPT1 = '1'                                 B04
     C                             AND W1OPT1 = ' '
     C                             OR FMOPT1 = '5'
     C                             AND W1OPT1 <> '1'
      *
      *          ------------------------------------------
      *          Option valid
      *          ------------------------------------------
     C                   MOVEL     FMOPT1        W1OPT1
     C                   ADD       1             W#VALC
      *
      *          ------------------------------------------
      *          Option 1 only single
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   IF        FMOPT1 = '1'                                 B05
     C                             OR FMOPT1 = '5'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EVAL      MSGID = 'OPTE027'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Option not valid
      *          ------------------------------------------
     C                   ELSE                                                   X05
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'OPTE001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
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
     C                   UPDATE    OMH00701
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH00701
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00701
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Store CRRN01
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                   Z-ADD     W#CRC1        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
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
     C                   READC     OMH00701
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
     C                   UPDATE    OMH00701
      *
      *          ------------------------------------------
      *          Do action if option is 1 or 5
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPT1 = '1'
     C                   EXSR      SRNEXT
     C                   WHEN      W#OPT1 = '5'
     C                   EXSR      SRSC03
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Option 1 requires only one processing moment
      *          ------------------------------------------
     C                   IF        W#OPT1 = '1'                                 B03
     C                   LEAVESR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed if no end requested.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   READC     OMH00701
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
     C     ENDPR         ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC03 - Screen 3 processing routine                        *
      *                                                             *
      * This routine displays the second screen.                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC03        BEGSR
      *
      *          ------------------------------------------
      *          Fill screen, format character strings
      *          ------------------------------------------
     C                   MOVEL     QUQUES        P#QUAD
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = C#NORM
     C                   MOVEL     P#QUAD        FMQUES
     C                   Z-ADD     FMANS#        P#ANS#
     C     $NS1K1        CHAIN     OMANSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     NSANSC        P#QUAD
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = C#NORM
     C                   MOVEL     P#QUAD        FMANSC
     C                   EXFMT     OMH00704
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC02 - Screen 2 processing routine                        *
      *                                                             *
      * This routine displays the second screen.                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      *IN41 = *ON
     C                   MOVEL     FMQUES        W#QUES
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
     C                   WRITE     OMH00703
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN02 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH00706
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = C#EXIT
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CRC2 = D#CRC#
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
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF02
     C                   EVAL      EROR02 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Do edit routine
      *          ------------------------------------------
     C                   EXSR      SRED02
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
     C                   IF        RRN02 <> *ZERO                               B02
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
     C                   MOVEL     W#QUES        FMQUES
     C                   EVAL      *IN41 = *OFF
      *
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRIN02 - Screen 2 initialisation                            *
      *                                                             *
      * This routine inits the second screen                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF02 - Work with answer refresh                               *
      *                                                             *
      * Actually this is only initialization                        *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
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
     C                   WRITE     OMH00706
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Build the subfile
      *          ------------------------------------------
     C                   EXSR      SRSF02
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSF02 - Subfile 2                                          *
      *                                                             *
      * This routine checks if the several attributes are  filled   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSF02        BEGSR
      *
      *          ------------------------------------------
      *          Do while not array of 1000 elements is empty
      *          ------------------------------------------
     C                   EVAL      IX = 1
     C                   EVAL      RRN02 = 0
     C                   DOW       QUE(IX) <> 0                                 B01
     C                             AND IX <= 1000                               B01
      *
      *          ------------------------------------------
      *          Get descriptions
      *          ------------------------------------------
     C                   EVAL      P#QUE# = QUE(IX)
     C                   EVAL      P#ANS# = ANS(IX)
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C     $NS1K1        CHAIN     OMANSR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPT2
     C                   Z-ADD     P#QUE#        FMQUE#
     C                   Z-ADD     P#ANS#        FMANS#
     C                   MOVEL     NSANSC        FMANSE
     C                   MOVEL     QUQUES        FMQUED
     C                   ADD       1             RRN02
     C                   ADD       1             IX
     C                   WRITE     OMH00705
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED02 - Edit the history screen                            *   *
      *                                                             *
      * This routine checks if the option fields in the subfile     *
      * where correctly filled. Answer and questions can be vieuwed *   *
      * (5) or positioned (8)                                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      CRRN02 = *ZEROS
     C                   EVAL      W#SAV# = RRN02
     C                   EVAL      W#VALC = *ZEROS
     C                   CLEAR                   W1OPT2
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   IF        RRN02 > *ZERO                                B01
     C                   READC     OMH00705
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
     C                   IF        FMOPT2 <> *BLANKS                            B03
      *
      *          ------------------------------------------
      *          Format the subfile option.
      *          ------------------------------------------
     C                   MOVEL     FMOPT2        W#OPT2
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPT2 = '8'                                 B04
     C                             AND W1OPT2 = ' '
     C                             OR FMOPT2 = '5'
     C                             AND W1OPT2 <> '8'
      *
      *          ------------------------------------------
      *          Option valid
      *          ------------------------------------------
     C                   MOVEL     FMOPT2        W1OPT2
     C                   ADD       1             W#VALC
      *
      *          ------------------------------------------
      *          Option 8 only single
      *          ------------------------------------------
     C                   ELSE                                                   X04
     C                   IF        FMOPT2 = '8'                                 B05
     C                             OR FMOPT2 = '5'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGID = 'OPTE028'
     C                   EXSR      SNDMSG
      *
      *          ------------------------------------------
      *          Option not valid
      *          ------------------------------------------
     C                   ELSE                                                   X05
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR02 = *ON
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'OPTE001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If an error was found, then position on the page of
      *          the first error
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
     C                   UPDATE    OMH00705
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   UPDATE    OMH00705
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH00705
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Store CRRN02
      *          ------------------------------------------
     C                   IF        CRRN02 = *ZEROS                              B01
     C                   Z-ADD     W#CRC2        CRRN02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR02 - Work with severity codes refresh                   *   *
      *                                                             *
      * This routine processes the display.                         *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      S#RR02 = RRN02
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN02 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH00705
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          If options is 5 then reloop.
      *          ------------------------------------------
     C                   IF        W#OPT2 <> '8'                                B03
     C                   EVAL      LOOP02 = *ON
     C                   ENDIF                                                  E03
     C                   EVAL      W#CRC2 = RRN02
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPT2        W#OPT2
     C                   MOVE      *BLANKS       FMOPT2
     C                   UPDATE    OMH00705
      *
      *          ------------------------------------------
      *          Do action if option is 5 or 8
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      W#OPT2 = '8'
     C                   EXSR      SRBACK
     C                   WHEN      W#OPT2 = '5'
     C                   EXSR      SRSC04
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Option 8 requires only one processing moment
      *          ------------------------------------------
     C                   IF        W#OPT2 = '8'                                 B03
     C                   LEAVESR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed if no end requested.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ELSE                                                   X03
     C                   READC     OMH00705
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
     C                   Z-ADD     W#CRC2        CRRN02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset record pointers
      *          ------------------------------------------
     C                   EVAL      RRN02 = S#RR02
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C     ENDPR2        ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRNEXT - Select next question                               *
      *                                                             *
      * This routine selects the next question                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRNEXT        BEGSR
      *
      *          ------------------------------------------
      *          Store current question, list and sequence
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   EVAL      IZ = %LOOKUP(00000:QUE:IZ)
     C                   IF        IZ > 0
     C                   EVAL      *IN99 = *ON
     C                   ELSE
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      IZ = 1
     C                   ENDIF
     C                   EVAL      QUE(IZ) = P#QUE#
     C                   Z-ADD     FMANS#        ANS(IZ)
     C                   EVAL      QLS(IZ) = P#QLS#
     C                   EVAL      QSQ(IZ) = P#QSQ#
     C                   IF        *IN40 = *OFF
     C                   EVAL      *IN40 = *ON
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Get next question
      *          ------------------------------------------
     C                   Z-ADD     FMANS#        P#ANS#
     C                   CALLP     OMH008 ( P#HEDC : P#QLS# : P#QSQ# :
     C                             P#QUE# : P#ANS# : P#STAT )
      *
      *          ------------------------------------------
      *          Failure,
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ENDSL
      *
      *          ------------------------------------------
      *          or continue: find possible answers
      *          ------------------------------------------
     C                   IF        P#STAT <> C#STOP                             B01
     C                             AND P#STAT <> C#NOTF
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   EVAL      EROR01 = *ON
      *
      *          ------------------------------------------
      *          Question not found: correct your database
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      P#STAT = C#NOTF
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Display the possible answers
      *          ------------------------------------------
     C                   EXSR      SRRF01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRBACK - Repostion in the question history                  *
      *                                                             *
      * This routine points at another question                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRBACK        BEGSR
      *
      *          ------------------------------------------
      *          Set the indicator, no errors anymore
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      W#POSI = *ON
      *
      *          ------------------------------------------
      *          Set the keylist
      *          ------------------------------------------
     C                   Z-ADD     RRN02         IZ
     C                   EVAL      P#QUE# = QUE(IZ)
     C                   EVAL      P#QLS# = QLS(IZ)
     C                   EVAL      P#QSQ# = QSQ(IZ)
     C                   FOR       MA_X = IZ TO %ELEM(QUE)
     C                   EVAL      QUE(MA_X) = *ZERO
     C                   ENDFOR
     C                   FOR       MA_X = IZ TO %ELEM(QLS)
     C                   EVAL      QLS(MA_X) = *ZERO
     C                   ENDFOR
     C                   FOR       MA_X = IZ TO %ELEM(QSQ)
     C                   EVAL      QSQ(MA_X) = *ZERO
     C                   ENDFOR
     C                   FOR       MA_X = IZ TO %ELEM(ANS)
     C                   EVAL      ANS(MA_X) = *ZERO
     C                   ENDFOR
      *
      *          ------------------------------------------
      *          Set the history indicator, when needed
      *          ------------------------------------------
     C                   IF        IZ = 1                                       B01
     C                   EVAL      *IN40 = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC04 - Screen 4 processing routine                        *
      *                                                             *
      * This routine displays the second screen.                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC04        BEGSR
      *
      *          ------------------------------------------
      *          Fill screen, format character strings
      *          ------------------------------------------
     C                   Z-ADD     FMQUE#        P#QUE#
     C                   Z-ADD     FMANS#        P#ANS#
     C     $QU1K1        CHAIN     OMHQUR
     C                   EVAL      *IN99 = NOT %FOUND
     C     $NS1K1        CHAIN     OMANSR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     NSANSC        P#QUAD
     C                   EVAL      W#STAT = P#STAT
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = W#STAT
     C                   MOVEL     P#QUAD        FMANSC
     C                   MOVEL     QUQUES        P#QUAD
     C                   EVAL      W#STAT = P#STAT
     C                   EVAL      P#POS# = 53
     C                   CALLP     OMX306 ( P#QUAD : P#POS# : P#STAT )
     C                   EVAL      P#STAT = W#STAT
     C                   MOVEL     P#QUAD        FMQUES
     C                   EXFMT     OMH00704
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRLAST - End of question processing                         *
      *                                                             *
      * No further questions: finish?                               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRLAST        BEGSR
      *
      *          ------------------------------------------
      *          Clear output buffer
      *          ------------------------------------------
     C                   EVAL      P#SEV1 = *BLANKS
     C                   EVAL      P#SEV2 = *BLANKS
     C                   EVAL      P#SEV3 = *BLANKS
     C                   EVAL      P#STRC = *BLANKS
     C                   EVAL      P#RES# = 0
     C                   EVAL      P#PRI# = 0
     C                   EVAL      P#ITPC = *BLANKS
     C                   EVAL      P#USF1 = *BLANKS
     C                   EVAL      P#USF2 = *BLANKS
     C                   EVAL      P#USF3 = 0
      *
      *          ------------------------------------------
      *          Retrieve original attributes
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   DOW       ANS(IZ) <> 0                                 B01
     C                             AND IZ <= 1000
     C                   EVAL      P#ANS# = ANS(IZ)
     C                   EVAL      P#QUE# = QUE(IZ)
     C                   EVAL      P#QSQ# = QSQ(IZ)
     C                   EVAL      P#QLS# = QLS(IZ)
     C     $LA1K2        CHAIN     OMQLAR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ADD       1             IZ
     C                   IF        LASEV1 <> *BLANKS                            B01
     C                   MOVEL     LASEV1        P#SEV1
     C                   ENDIF                                                  E01
     C                   IF        LASEV2 <> *BLANKS                            B01
     C                   MOVEL     LASEV2        P#SEV2
     C                   ENDIF                                                  E01
     C                   IF        LASEV3 <> *BLANKS                            B01
     C                   MOVEL     LASEV3        P#SEV3
     C                   ENDIF                                                  E01
     C                   IF        LASTRC <> *BLANKS                            B01
     C                   MOVEL     LASTRC        P#STRC
     C                   ENDIF                                                  E01
     C                   IF        LARES# <> *ZEROS                             B01
     C                   Z-ADD     LARES#        P#RES#
     C                   ENDIF                                                  E01
     C                   IF        LAPRI# <> *ZEROS                             B01
     C                   Z-ADD     LAPRI#        P#PRI#
     C                   ENDIF                                                  E01
     C                   IF        LAITPC <> *BLANKS                            B01
     C                   MOVEL     LAITPC        P#ITPC
     C                   ENDIF                                                  E01
     C                   IF        LAUSF1 <> *BLANKS                            B01
     C                   MOVEL     LAUSF1        P#USF1
     C                   ENDIF                                                  E01
     C                   IF        LAUSF2 <> *BLANKS                            B01
     C                   MOVEL     LAUSF2        P#USF2
     C                   ENDIF                                                  E01
     C                   IF        LAUSF3 <> *ZEROS                             B01
     C                   Z-ADD     LAUSF3        P#USF3
     C                   ENDIF                                                  E01
     C                   ENDDO
      *
      *          ------------------------------------------
      *          If 'Write to history' is on
      *          ------------------------------------------
     C                   IF        P#ACTN = C#HIST                              B01
      *
      *          ------------------------------------------
      *          Update history file, fill parameters
      *          ------------------------------------------
     C                   MOVEL     P#HEDC        AHHEDC
     C                   MOVEL     P#INC#        AHINC#
     C                   Z-ADD     QLS(1)        AHOQL#
      *
      *          ------------------------------------------
      *          Initialize sequence field
      *          ------------------------------------------
     C                   EVAL      AHSEQ# = *ZEROS
      *
      *          ------------------------------------------
      *          Loop through all questions and answers
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   DOW       ANS(IZ) <> 0                                 B02
     C                             AND IZ <= 1000
     C                   Z-ADD     ANS(IZ)       AHANS#
     C                   Z-ADD     QUE(IZ)       AHQUE#
     C                   Z-ADD     QSQ(IZ)       AHQSQ#
     C                   Z-ADD     QLS(IZ)       AHQLS#
     C                   ADD       10            AHSEQ#
     C                   WRITE     OMQAHR
     C                   ADD       1             IZ
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Call OMH929 (fill log file)
      *          ------------------------------------------
     C                   IF        W#STAT = C#STOP                              B02
     C                   EVAL      MSGID = 'MH00701'
     C                   ELSE                                                   X02
     C                   IF        W#STAT = C#NOTF                              B03
     C                   EVAL      MSGID = 'MH00702'
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   EXSR      SNDMSG
     C                   EVAL      P#MSID = MSGID
     C                   EVAL      %SUBST(P#MSGF:1:6) = 'OMHMSG'
     C                   EVAL      P#ACTC = '60'
     C                   CALLP     OMX929 ( P#HEDC : P#INC# : P#ACTC :
     C                             P#MSID : P#MSGD : P#MSGF : P#STAT )
      *
      *          ------------------------------------------
      *          Else (action is 'Ask questions')
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Save original return status in workfield
      *          ------------------------------------------
     C                   EVAL      W#STAT = P#STAT
      *
      *          ------------------------------------------
      *          Fill return status
      *          ------------------------------------------
     C                   IF        P#STAT = C#NOTF                              B02
     C                   EVAL      P#STAT = P#STAT
     C                   ELSE                                                   X02
     C                   EVAL      P#STAT = C#NORM
     C                   ENDIF                                                  E02
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of program
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
      *          Parameter list for OMH929 (Fill log file)
      *          ------------------------------------------
     C     $MX929        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#INC#
     C                   PARM      '60'          P#ACTC
     C                   PARM                    P#MSID
     C                   PARM                    P#MSGD
     C                   PARM                    P#MSGF
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH008 (Get question)
      *          ------------------------------------------
     C     $MH008        PLIST
     C                   PARM                    P#HEDC
     C                   PARM                    P#QLS#
     C                   PARM                    P#QSQ#
     C                   PARM                    P#QUE#
     C                   PARM                    P#ANS#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMX306 (Format string)
      *          ------------------------------------------
     C     $MX306        PLIST
     C                   PARM                    P#QUAD
     C                   PARM      53            P#POS#
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          OMHQUL1 Question file
      *          ------------------------------------------
     C     $QU1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QUE#
      *
      *          ------------------------------------------
      *          OMQLAL1 Question list answer file
      *          ------------------------------------------
     C     $LA1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
     C                   KFLD                    P#QSQ#
     C                   KFLD                    P#QUE#
      *
     C     $LA1K2        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QLS#
     C                   KFLD                    P#QSQ#
     C                   KFLD                    P#QUE#
     C                   KFLD                    P#ANS#
      *
      *          ------------------------------------------
      *          OMANSL1 Answer file
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    P#HEDC
     C                   KFLD                    P#QUE#
     C                   KFLD                    P#ANS#
      *
      *          ------------------------------------------
      *          Dummy read QAH
      *          ------------------------------------------
     C                   IF        1 = 2                                        B01
     C                   READ      OMQAHR
     C                   EVAL      *IN99 = %EOF
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
      *          If number of parameters is zero, or action
      *          is 'Write to history' is on, then the
      *          program can be closed
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                             OR P#ACTN = C#HIST
     C                             OR P#ACTN = C#TEST
     C                   EVAL      *INLR = *ON
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Return to calling program
      *          ------------------------------------------
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
     C                   FOR       MS = 1 TO 2                                  B01
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
      *          Fill message got indicator
      *          ------------------------------------------
     C                   EVAL      MSGGET = *ON
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
     C                   DUMP
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMH007D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMANSL1 '                          X02
     C                   MOVEL     ANSL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQLAL1 '                          X02
     C                   MOVEL     QLAL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHQUL1 '                          X02
     C                   MOVEL     HQUL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMQAH   '                          X02
     C                   MOVEL     QAH           P#FILE
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
      *
**
OPT0008  -  '1' or '5'                        1
OPT0055  -  '5' or '8'                        2
