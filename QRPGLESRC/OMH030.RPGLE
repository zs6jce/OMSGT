     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH030.                                       *
      * Function ...: Answer list maintenance                       *
      * Author .....: Remain Software                               *
      * Module .....: HelpDesk                                      *
      * Parameters .: P#HEDC    -  HelpDesk code                    *
      *               P#STAT    -  Status                           *
      *                                                             *
      * This program allows helpdesk managers + the secofr to       *
      * enter the HelpDesk answer lists.                            *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMAZHL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(AZHL1)
     FOMAZDL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(AZDL1)
     FOMANSL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(ANSL1)
     FOMHQUL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HQUL1)
     FOMHQLL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HQLL1)
     FOMH030D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
     F                                     SFILE(OMH03001:RRN01)
     F                                     SFILE(OMH03005:RRN03)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * Extension spec's                                             *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(20)
     D MSD             S              7    DIM(20) CTDATA PERRCD(1)
     D MC              S              1    DIM(256)
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
      *          File information data structures
      *          ------------------------------------------
     D AZHL1           DS            57
     D AZDL1           DS            57
     D HQUL1           DS            57
     D ANSL1           DS            57
     D HQLL1           DS            57
      *
      *          ------------------------------------------
      *          Screen information data structure
      *          ------------------------------------------
     D INFO            DS
     D  STATUS           *STATUS
     D  D#CRC#               378    379B 0
      *
      *          ------------------------------------------
      *          Binary number formatting for message data.
      *          ------------------------------------------
     D                 DS
     D D#BIN4                         9B 0
     D  D#BIA4                        4    OVERLAY(D#BIN4)
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
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
     D C#TEST          C                   CONST('*TEST')
      *
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
      * Prototype for OMH030
     D OMH030          PR
     D P#HEDC_                             LIKE(P#HEDC)
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
      * Prototype for 'OMX105'
     D OMX105          PR                  EXTPGM('OMX105')
     D FMHEDC_                             LIKE(FMHEDC)
     D HEDD_                               LIKE(HEDD)
      * Prototype for 'OMX121'
     D OMX121          PR                  EXTPGM('OMX121')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMQUE#_                             LIKE(FMQUE#)
     D INDI_                               LIKE(INDI)
     D QUES_                               LIKE(QUES)
      * Prototype for 'OMX122'
     D OMX122          PR                  EXTPGM('OMX122')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMQUE#_                             LIKE(FMQUE#)
     D FMANS#_                             LIKE(FMANS#)
     D ANSC_                               LIKE(ANSC)
      * Prototype for 'OMX120'
     D OMX120          PR                  EXTPGM('OMX120')
     D FMHEDC_                             LIKE(FMHEDC)
     D FMOQL#_                             LIKE(FMOQL#)
     D DESC_                               LIKE(DESC)
      * Prototype for 'OMX305'
     D OMX305          PR                  EXTPGM('OMX305')
     D P#QOAI_                             LIKE(P#QOAI)
     D FMQUE2_                             LIKE(FMQUE2)
     D FMANS2_                             LIKE(FMANS2)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (Send message)
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
      *          OMHQL Question list file on question list
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
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH029 (Get info)
      * *ENTRY Interface for Main Procedure
     D OMH030          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D p#pri#          S              3P 0
     D p#qls#          S              5P 0
     D p#res#          S              3P 0
     D p#usf3          S             15P 0
     D w#cr03          S              5P 0
     D w#cr04          S              5P 0
     D EROR01          S                   LIKE(INDI)
     D EROR02          S                   LIKE(INDI)
     D EROR03          S                   LIKE(INDI)
     D EROR04          S                   LIKE(INDI)
     D I#PSSR          S              1
     D I#SC05          S                   LIKE(INDI)
     D LOOP01          S                   LIKE(INDI)
     D LOOP02          S                   LIKE(INDI)
     D LOOP03          S                   LIKE(INDI)
     D LOOP04          S                   LIKE(INDI)
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#ACTN          S                   LIKE(ACTN)
     D P#ANSC          S                   LIKE(ANSC)
     D P#CDE#          S                   LIKE(CTN#)
     D P#CTN#          S                   LIKE(CTN#)
     D P#FILE          S             57
     D P#HEDD          S                   LIKE(HEDD)
     D P#INC#          S                   LIKE(INC#)
     D P#ITPC          S                   LIKE(ITPC)
     D P#MSTA          S             40
     D P#OPTI          S                   LIKE(INDI)
     D P#QOAI          S                   LIKE(INDI)
     D P#QUES          S                   LIKE(QUES)
     D P#SEV1          S                   LIKE(SEV1)
     D P#SEV2          S                   LIKE(SEV2)
     D P#SEV3          S                   LIKE(SEV3)
     D P#STRC          S                   LIKE(STRC)
     D P#SUL#          S                   LIKE(SUL#)
     D P#USF1          S                   LIKE(USF1)
     D P#USF2          S                   LIKE(USF2)
     D RRN01           S              5  0
     D RRN03           S              5  0
     D S#HEDC          S                   LIKE(HEDC)
     D W#CR01          S              5  0
     D W#IX            S              5  0
     D W#MSGI          S              1
     D W#OPTI          S                   LIKE(FMOPTI)
     D W#OPT2          S                   LIKE(FMOPT2)
     D W#SAV#          S              5  0
     D W#VALC          S              5  0
     D W#WRKI          S              1
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
      *          Do the processing routine if authorised
      *          ------------------------------------------
     C                   SELECT
     C                   WHEN      P#STAT = C#NORM
     C                   EXSR      SRVERW
     C                   ENDSL
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
      * PROMPT - Prompt a field                                      * /
      *                                                              * /
      * This routine prompts a field.                                * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     PROMPT        BEGSR
      *
      *          ------------------------------------------
      *          Display 1.
      *          HelpDesk code
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03002'                          B01
     C                             AND FMFLDC = 'FMHEDC'                        B02
     C                   MOVE      FMHEDC        S#HEDC
     C                   CALLP     OMX105 ( FMHEDC : HEDD )
     C                   IF        FMHEDC <> S#HEDC                             B02
     C                   EXSR      SRCHGH
     C                   ENDIF                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Display 4.
      *          Question
      *          ------------------------------------------
     C                   IF        FMFMTC = 'OMH03008'                          B02
     C                   IF        FMFLDC = 'FMQUE#'                            B03
     C                   CALLP(E)  OMX121 ( FMHEDC : FMQUE# : INDI : QUES )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMQUE1 = QUES
     C                   ENDIF
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Display 4.
      *          Answer
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMANS#'                            B04
     C                             AND *IN80 = *OFF
     C                             AND *IN80 = *OFF
     C                   CALLP(E)  OMX122 ( FMHEDC : FMQUE# : FMANS# : ANSC
     C                             )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMANS1 = ANSC
     C                   ENDIF
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Question list.
      *          ------------------------------------------
     C                   IF        FMFLDC = 'FMOQL#'                            B05
     C                   CALLP(E)  OMX120 ( FMHEDC : FMOQL# : DESC )
     C                   IF        %ERROR = *OFF
     C                   EVAL      FMDES1 = DESC
     C                   ENDIF
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Invalid prompt location.
      *          ------------------------------------------
     C                   EVAL      MSGID = 'PMT0002'
     C                   EXSR      SNDMSG
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
      * SRVERW - Main program processing routine                    *
      *                                                             *
      * This routine calls the first screen routine until the field *
      * P#STAT is filled.                                           *
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
      * SRSC01 - Work with answer lists                             *
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
     C                   WRITE     OMH03003
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN01 = *ZEROS                               B03
     C                   EVAL      *IN72 = *ON
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH03002
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CR01 = D#CRC#
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
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          CMD-12 is CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Change the HelpDesk if required.
      *          ------------------------------------------
     C                   IF        S#HEDC <> FMHEDC                             B05
     C                   EXSR      SRCHGH
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          F6=Add answer list
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B06
     C                   EXSR      SRSC02
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X06
      *
      *          ------------------------------------------
      *          Edit
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
     C                   MOVE      *BLANKS       FMPOS
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF01 - Work with question lists  - refresh                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      S#HEDC        FMHEDC
     C                   EVAL      RRN01 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH03002
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first security record
      *          ------------------------------------------
     C     FMHEDC        SETLL     OMAZHL1
     C     FMHEDC        READE(N)  OMAZHL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      AZALSC        FMALSC
     C                   MOVE      AZALSD        FMALSD
     C                   ADD       1             RRN01
     C                   WRITE     OMH03001
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     FMHEDC        READE(N)  OMAZHL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
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
      * SRED01 - Work with question lists - edit                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN01 = *ZEROS
     C                   EVAL      W#MSGI = *OFF
     C                   EVAL      W#SAV# = RRN01
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH03001
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
     C                   MOVE      FMOPTI        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B02
     C                   MOVEL     FMOPTI        W#WRKI
     C                   MOVE      *BLANKS       FMOPTI
     C                   MOVE      W#WRKI        FMOPTI
     C                   ENDIF                                                  E02
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
     C                   IF        FMOPTI = ' 2'                                B03
     C                             OR FMOPTI = ' 4'
     C                             OR FMOPTI = '12'
      *
      *          ------------------------------------------
      *          Delete, still details?
      *          ------------------------------------------
     C                   IF        FMOPTI = ' 4'                                B04
     C     $ZD1K1        SETLL     OMAZDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B05
     C                   EVAL      MSGID = 'DLT0029'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active in file if not then error, else ok.
      *          ------------------------------------------
     C                   IF        EROR01 = *OFF                                B04
     C     $ZD1K1        SETLL     OMAZHL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B05
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X05
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
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
      *          Send the message
      *          ------------------------------------------
     C                   IF        W#MSGI = *OFF                                B04
     C                   MOVEL(P)  FMOPTI        MSGDTA
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
     C                   UPDATE    OMH03001
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH03001
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH03001
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          If subfile must be positioned, and no errors occured
      *          then position it.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B01
     C                             AND FMPOS <> *BLANKS
     C                   FOR       W#IX = 1 TO W#SAV#                           B02
     C     W#IX          CHAIN     OMH03001
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        FMALSC >= FMPOS                              B03
     C                   Z-ADD     W#IX          CRRN01
     C                   EVAL      W#IX = W#SAV#
     C                   MOVE      *BLANKS       FMPOS
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
     C                   ENDFOR                                                 E02
      *
     C                   IF        FMPOS <> *BLANKS                             B02
     C                   MOVE      *BLANKS       FMPOS
     C                   Z-ADD     W#SAV#        CRRN01
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E02
      *
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   IF        CRRN01 = *ZEROS                              B02
     C                   Z-ADD     W#CR01        CRRN01
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset overall relative record number
      *          ------------------------------------------
     C                   EVAL      RRN01 = W#SAV#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Work with question lists - process                 *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      W#IX = 1
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN01 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH03001
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options where typed.
      *          ------------------------------------------
     C                   EVAL      LOOP01 = *ON
     C                   EVAL      W#CR01 = RRN01
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPTI        W#OPTI
     C                   MOVE      *BLANKS       FMOPTI
     C                   UPDATE    OMH03001
      *
      *          ------------------------------------------
      *          Fill the description
      *          ------------------------------------------
     C                   MOVEL     FMALSD        FMALS1
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   SELECT                                                 B03
     C                   WHEN      W#OPTI = '12'                                X03
     C                   EXSR      SRSC03
     C                   OTHER                                                  X03
     C                   EXSR      SRSC02
     C                   ENDSL                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH03001
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
     C                   Z-ADD     W#CR01        CRRN01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC02 - Maintain answer list                               *
      *                                                             *
      * This routine displays the second screen. The flow of the    *
      * screen is slightly different then normal. The change, add   *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81. 80 controls the protection of the   *
      * question list, and 81 controls the protection of the        *
      * description.                                                *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
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
     C                   EXFMT     OMH03004
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
     C                   EVAL      EROR02 = *OFF
     C                   EVAL      LOOP02 = *OFF
      *
      *          ------------------------------------------
      *          F3=Exit
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do edit routine if not F3 or F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B03
     C                   EXSR      SRED02
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
      * SRIN02 - Maintain answerlist init                           *
      *                                                             *
      * This routine blanks the input fields on the screen.         *
      * Pending on the required action.           The change, add   *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81. 80 controls the protection of the   *
      * question list, and 81 controls the protection of the        *
      * description.                                                *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN02        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X01
     C                   IF        W#OPTI = ' 2'                                B02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
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
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF02 - Maintain answer list refresh                       *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF02        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields if addition
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      *BLANKS       FMALSC
     C                   MOVE      *BLANKS       FMALSD
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get record if change or delete
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C     $ZD1K1        CHAIN(N)  OMAZHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVE      AZALSC        FMALSC
     C                   MOVE      AZALSD        FMALSD
     C                   ENDIF                                                  E01
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
      * This routine checks if the data is ok.                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED02        BEGSR
      *
      *          ------------------------------------------
      *          Test if possible
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
      *
      *          ------------------------------------------
      *          Check if the record exists
      *          ------------------------------------------
     C     $ZD1K1        SETLL     OMAZHL1
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
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR02 - Process screen 2                                   *
      *                                                             *
      * This routine updates the answer list file                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR02        BEGSR
      *
      *          ------------------------------------------
      *          Loop again if fields where changed.
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP02
      *
      *          ------------------------------------------
      *          Delete the file if wanted.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C     $ZD1K1        DELETE    OMAZHL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do if something changed.
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     $ZD1K1        CHAIN     OMAZHL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVE      FMHEDC        AZHEDC
     C                   MOVE      FMALSC        AZALSC
     C                   MOVE      FMALSD        AZALSD
      *
      *          ------------------------------------------
      *          Add or update the file. Fill description only on add.
      *          *IN99=*OFF record found in previous chain
      *          *IN99=*ON  record not found in previous chain
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B03
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMAZHR
     C                   ENDIF
     C                   ELSE                                                   X03
     C                   IF        *IN99 = *ON
     C                   WRITE     OMAZHR
     C                   ENDIF
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          If in ADD mode then force CHANGE mode.
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
      * SRSC03 - Work with questions and answers                    *
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
      *          Display the function keys
      *          ------------------------------------------
     C                   WRITE     OMH03007
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   IF        RRN03 = *ZEROS                               B03
     C                   EVAL      *IN90 = *OFF
     C                   ELSE                                                   X03
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  E03
     C                   EXFMT     OMH03006
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CR03 = D#CRC#
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
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR03 = *OFF
     C                   EVAL      LOOP03 = *OFF
      *
      *          ------------------------------------------
      *          CMD-12 is leave, CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF03
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          F6=Add
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B05
     C                   EXSR      SRSC04
     C                   EVAL      EROR03 = *ON
     C                   ELSE                                                   X05
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED03
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
      *          CMD-12 is leave
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   EXSR      SRPR03
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
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN03        BEGSR
      *
      *          ------------------------------------------
      *          Clear fields
      *          ------------------------------------------
     C                   EVAL      S#HEDC = P#HEDC
      *
      *          ------------------------------------------
      *          Do refresh subroutine
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
      * SRRF03 - Work with questions and answers                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   MOVE      S#HEDC        FMHEDC
     C                   EVAL      RRN03 = *ZEROS
      *
      *          ------------------------------------------
      *          Initialize subfile
      *          ------------------------------------------
     C                   EVAL      *IN91 = *ON
     C                   WRITE     OMH03006
     C                   EVAL      *IN91 = *OFF
      *
      *          ------------------------------------------
      *          Read first question list question
      *          ------------------------------------------
     C     $ZD1K1        SETLL     OMAZDL1
     C     $ZD1K1        READE(N)  OMAZDL1
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Fill subfile record
      *          ------------------------------------------
     C                   MOVE      *BLANKS       FMOPT2
      *
      *                    --------------------------------
      *                    Get the question
      *                    --------------------------------
     C                   MOVE      ZDQUE#        FMQUE#
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     QUQUES        FMQUES
     C                   MOVEL     QUQUES        FMQUE2
     C                   ELSE                                                   X02
     C                   MOVEL(P)  *ALL'?*'      FMQUES
     C                   MOVE      *BLANKS       FMQUE2
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the answer
      *                    --------------------------------
     C                   MOVE      ZDANS#        FMANS#
     C                   IF        FMANS# > *ZEROS                              B02
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEL     NSANSC        FMANSC
     C                   MOVEL     NSANSC        FMANS2
     C                   ELSE                                                   X03
     C                   MOVEL(P)  *ALL'?*'      FMANSC
     C                   MOVE      *BLANKS       FMANS2
     C                   ENDIF                                                  E03
      *
     C                   ELSE                                                   X02
     C                   MOVEL(P)  '*ANY'        FMANSC
     C                   MOVE      *BLANKS       FMANS2
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the question list
      *                    --------------------------------
     C                   Z-ADD     ZDOQL#        FMOQL#
     C                   IF        FMOQL# > *ZEROS                              B02
     C     $QL1K1        CHAIN     OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF
     C                   MOVEL     QLDESC        FMDESC
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   MOVE(P)   *ALL'*?'      FMDESC
     C                   ENDIF
     C                   ELSE                                                   X02
     C                   MOVEL(P)  '*ANY'        FMDESC
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Write the subfile record
      *                    --------------------------------
     C                   ADD       1             RRN03
     C                   WRITE     OMH03005
      *
      *          ------------------------------------------
      *          Read next record
      *          ------------------------------------------
     C     $ZD1K1        READE(N)  OMAZDL1
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill current subfile relative record number
      *          to display first subfile page
      *          ------------------------------------------
     C                   Z-ADD     1             CRRN03
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRED03 - Work with questions & answers - edit               *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED03        BEGSR
      *
      *          ------------------------------------------
      *          Init test
      *          ------------------------------------------
     C                   EVAL      W#VALC = *ZEROS
     C                   EVAL      W#IX = 1
     C                   EVAL      CRRN03 = *ZEROS
     C                   EVAL      W#MSGI = *OFF
     C                   EVAL      W#SAV# = RRN03
      *
      *          ------------------------------------------
      *          Read first changed subfile record
      *          ------------------------------------------
     C                   READC(E)  OMH03005
     C                   EVAL      *IN99 = %ERROR OR %EOF
      *
      *          ------------------------------------------
      *          Do while not end of file
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B01
     C                   MOVE      FMOPT2        W#WRKI
     C                   IF        W#WRKI = *BLANKS                             B02
     C                   MOVEL     FMOPT2        W#WRKI
     C                   MOVE      *BLANKS       FMOPT2
     C                   MOVE      W#WRKI        FMOPT2
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Do if option is not blank
      *          ------------------------------------------
     C                   IF        FMOPT2 <> *BLANKS                            B02
      *
      *          ------------------------------------------
      *          If option is valid, then check the number of valid
      *          codes. *IN93 is the subfile next change indicator
      *          ------------------------------------------
     C                   IF        FMOPT2 = '4'                                 B03
     C                             OR FMOPT2 = '5'                              B03
      *
      *          ------------------------------------------
      *          Option is valid, check if subfile record is still
      *          active in file if not then error, else ok.
      *          ------------------------------------------
     C     $ZD1K2        SETLL     OMAZDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *OFF                                 B04
     C                   EVAL      MSGID = 'FILE002'
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR03 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X04
     C                   ADD       1             W#VALC
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          If option is other than valid, then field is in error
      *          Reverse screen fields to indicate this
      *          ------------------------------------------
     C                   ELSE                                                   X03
     C                   EVAL      *IN70 = *ON
     C                   EVAL      EROR03 = *ON
      *
      *          ------------------------------------------
      *          Send the message
      *          ------------------------------------------
     C                   IF        W#MSGI = *OFF                                B04
     C                   MOVEL(P)  FMOPTI        MSGDTA
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
     C                   IF        CRRN03 = *ZEROS                              B03
     C                             AND *IN70 = *ON
     C                   Z-ADD     RRN03         CRRN03
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Update subfile
      *          ------------------------------------------
     C                   EVAL      *IN93 = *ON
     C                   UPDATE    OMH03005
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN70 = *OFF
      *
      *          ------------------------------------------
      *          Selection is blanks, reset error
      *          ------------------------------------------
     C                   ELSE                                                   X02
     C                   UPDATE    OMH03005
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Read next subfile record
      *          ------------------------------------------
     C                   READC     OMH03005
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Fill the current relative record on the page to
      *          display.
      *          ------------------------------------------
     C                   IF        CRRN03 = *ZEROS                              B01
     C                   Z-ADD     W#CR03        CRRN03
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Reset overall relative record number
      *          ------------------------------------------
     C                   EVAL      RRN03 = W#SAV#
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR03 - Work with questions and answers, process           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR03        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EVAL      W#IX = 1
      *
      *          ------------------------------------------
      *          Read subfile next changed record
      *          If records in subfile and valid codes found.
      *          ------------------------------------------
     C                   IF        RRN03 > *ZEROS                               B01
     C                             AND W#VALC > *ZEROS
     C                   READC     OMH03005
     C                   EVAL      *IN99 = %EOF
      *
      *          ------------------------------------------
      *          Read subfile until valid option found
      *          ------------------------------------------
     C                   DOW       *IN99 = *OFF                                 B02
      *
      *          ------------------------------------------
      *          Loop again for options where typed.
      *          ------------------------------------------
     C                   EVAL      LOOP03 = *ON
     C                   EVAL      W#CR03 = RRN03
      *
      *          ------------------------------------------
      *          Clear subfile option
      *          ------------------------------------------
     C                   MOVE      FMOPT2        W#OPT2
     C                   MOVE      *BLANKS       FMOPT2
     C                   UPDATE    OMH03005
      *
      *          ------------------------------------------
      *          Do action
      *          ------------------------------------------
     C                   IF        W#OPT2 <> '5'                                B03
     C                   EXSR      SRSC04
      *
      *          ------------------------------------------
      *          5=Display question & answer
      *          ------------------------------------------
     C                   ELSE                                                   X03
      *
     C                   IF        FMANS2 <> *BLANKS                            B04
     C                   EVAL      P#QOAI = '3'
     C                   ELSE                                                   X04
     C                   EVAL      P#QOAI = '1'
     C                   ENDIF                                                  E04
      *
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMX305 ( P#QOAI : FMQUE2 : FMANS2 :
     C                             P#STAT )
      *
     C                   IF        P#STAT = C#EXIT                              B04
     C                   EVAL      *INKC = *ON
     C                   ENDIF                                                  E04
      *
     C                   IF        P#STAT = C#CNCL                              B04
     C                   EVAL      *INKL = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Exit if F3
      *          ------------------------------------------
     C                   IF        *INKC = *ON                                  B03
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Leave if F12.
      *          ------------------------------------------
     C                   IF        *INKL = *ON                                  B03
     C                   LEAVE
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Read next changed
      *          ------------------------------------------
     C                   READC     OMH03005
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
     C                   Z-ADD     W#CR03        CRRN03
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRSC04 - Maintain question and answer                       *
      *                                                             *
      * This display is used to maintain the question which is      *
      * connected to the question list. The indicator settings      *
      * are.                                                        *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRSC04        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init
      *          ------------------------------------------
     C                   EXSR      SRIN04
      *
      *          ------------------------------------------
      *          Do while no end
      *          ------------------------------------------
     C                   DOU       LOOP04 = *OFF                                B01
      *
      *          ------------------------------------------
      *          Do while errors in screen
      *          ------------------------------------------
     C                   DOU       EROR04 = *OFF                                B02
      *
      *          ------------------------------------------
      *          Display the message subfile
      *          ------------------------------------------
     C                   WRITE     MSGCTL
      *
      *          ------------------------------------------
      *          Display the subfile control & subfile if records found
      *          ------------------------------------------
     C                   EXFMT     OMH03008
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
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      EROR04 = *OFF
     C                   EVAL      LOOP04 = *OFF
      *
      *          ------------------------------------------
      *          CMD-12 is leave, CMD-3 is exit
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Save current relative record number
      *          ------------------------------------------
     C                   EVAL      W#CR04 = D#CRC#
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F4=Prompt
      *          ------------------------------------------
     C                   IF        *INKD = *ON                                  B03
     C                   EXSR      PROMPT
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          F5=Refresh screen
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B04
     C                   EXSR      SRRF04
     C                   EVAL      EROR04 = *ON
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Edit
      *          ------------------------------------------
     C                   EXSR      SRED04
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Re-enter error loop if errors or command key
      *          ------------------------------------------
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          CMD-12 is leave
      *          ------------------------------------------
     C                   IF        *INKL = *ON
     C                   LEAVE
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Do processing
      *          ------------------------------------------
     C                   EXSR      SRPR04
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
      * SRIN04 - Screen 4 initialisation                            *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN04        BEGSR
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C                   IF        *INKF = *ON                                  B01
     C                   MOVEL     VAR(1)        FMDES1
     C                   MOVEL     VAR(2)        FMQUE1
     C                   MOVEL     VAR(1)        FMANS1
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X01
      *
     C                   MOVEL     FMALSD        FMALS1
     C                   IF        W#OPT2 = '2'                                 B02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ELSE                                                   X02
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *ON
      *
      *          ------------------------------------------
      *          Display deletion message
      *          ------------------------------------------
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Do refresh subroutine
      *          ------------------------------------------
     C                   EXSR      SRRF04
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRRF04 - Work with Q&A                                      *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF04        BEGSR
      *
      *          ------------------------------------------
      *          Blank fields if add.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   EVAL      FMOQL# = *ZEROS
     C                   EVAL      FMANS# = *ZEROS
     C                   EVAL      FMQUE# = *ZEROS
      *
      *          ------------------------------------------
      *          Else fill the key and read answer and actions records
      *          ------------------------------------------
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Read first answer for this question & answer
      *          ------------------------------------------
     C     $ZD1K3        CHAIN(N)  OMAZDL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *                    --------------------------------
      *                    Get the originating question list
      *                    --------------------------------
     C                   IF        FMOQL# <> *ZEROS                             B02
     C     $QL1K1        CHAIN     OMHQLL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     QLDESC        FMDES1
     C                   ELSE                                                   X02
     C                   MOVEL(P)  VAR(1)        FMDES1
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the originating question list
      *                    --------------------------------
     C                   IF        FMOQL# <> *ZEROS                             B02
     C     $QU1K1        CHAIN     OMHQUL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     QUQUES        FMQUE1
     C                   ELSE                                                   X02
     C                   MOVEL(P)  VAR(2)        FMQUE1
     C                   ENDIF                                                  E02
      *
      *                    --------------------------------
      *                    Get the answer
      *                    --------------------------------
     C                   IF        FMANS# <> *ZEROS                             B02
     C     $NS1K1        CHAIN     OMANSL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   MOVEL     NSANSC        FMANS1
     C                   ELSE                                                   X02
     C                   MOVEL(P)  VAR(1)        FMANS1
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
      * SRED04 - Work with Answers edit                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED04        BEGSR
      *
      *          ------------------------------------------
      *          Question may not be zero.
      *          ------------------------------------------
     C                   IF        FMQUE# = *ZEROS                              B01
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      MSGID = 'ZER0001'
     C                   EVAL      EROR04 = *ON
     C                   EVAL      *IN71 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Record may not already exist.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C     $ZD1K3        SETLL     OMAZDL1
     C                   EVAL      *IN99 = %EQUAL
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      MSGID = 'FILE001'
     C                   EVAL      EROR04 = *ON
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EXSR      SNDMSG
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
      * SRPR04 - Work with Answers process                          *
      *                                                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR04        BEGSR
      *
      *          ------------------------------------------
      *          If this is add.
      *          ------------------------------------------
     C                   IF        *IN80 = *OFF                                 B01
     C                             AND *IN81 = *OFF
     C                   MOVE      FMHEDC        ZDHEDC
     C                   MOVE      FMALSC        ZDALSC
     C                   Z-ADD     FMQUE#        ZDQUE#
     C                   Z-ADD     FMANS#        ZDANS#
     C                   Z-ADD     FMOQL#        ZDOQL#
     C                   WRITE     OMAZDR
     C                   EVAL      LOOP04 = *ON
     C                   EVAL      *IN80 = *ON
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          If this is delete.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B02
     C                             AND *IN81 = *ON
     C     $ZD1K3        DELETE    OMAZDL1
     C                   EVAL      *IN99 = NOT %FOUND
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
      * SRCHGH - Change the Helpdesk                                *
      *                                                             *
      * This routine changes the helpdesk                           *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRCHGH        BEGSR
      *
      *          ------------------------------------------
      *          If the HelpDesk code is blanks, then reset it.
      *          ------------------------------------------
     C                   IF        FMHEDC = *BLANKS                             B01
     C                   MOVE      S#HEDC        FMHEDC
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Change the helpdesk
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   MOVE      FMHEDC        P#HEDC
     C                   CALLP     OMH029 ( P#OPTI : P#HEDC : P#SUL# :
     C                             P#STAT : D#D7HD : D#D8HD )
      *
      *          ------------------------------------------
      *          If errors, reset
      *          ------------------------------------------
     C                   IF        P#STAT <> C#NORM                             B02
     C                   EVAL      *IN71 = *ON
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Fill screen fields from parameters
      *          ------------------------------------------
     C                   MOVE      P#HEDC        FMHEDC
     C                   MOVEL     D8HEDD        FMHEDD
     C                   EXSR      SRIN01
     C                   EVAL      EROR01 = *ON
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
      *          Save field definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
     C                   EVAL      W#CR01 = *ZEROS
      *
      *          ------------------------------------------
      *          Indicator definitions
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER
      *
      *          ------------------------------------------
      *          Parameter list for OMX305 (Display Q & A string)
      *          ------------------------------------------
     C     $MX305        PLIST
     C                   PARM                    P#QOAI
     C                   PARM                    FMQUE2
     C                   PARM                    FMANS2
     C                   PARM      C#NORM        P#STAT
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
     C                   PARM                    MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
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
      *          OMHQL Question list file on question list
      *          ------------------------------------------
     C     $QL1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMOQL#
      *
      *          ------------------------------------------
      *          OMAZD Answer list details
      *          ------------------------------------------
     C     $ZD1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMALSC
      *
     C     $ZD1K2        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMALSC
     C                   KFLD                    FMQUE#
      *
     C     $ZD1K3        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMALSC
     C                   KFLD                    FMQUE#
     C                   KFLD                    FMANS#
      *
      *          ------------------------------------------
      *          OMHQU Question file on question
      *          ------------------------------------------
     C     $QU1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMQUE#
      *
      *          ------------------------------------------
      *          OMANS Answers
      *          ------------------------------------------
     C     $NS1K1        KLIST
     C                   KFLD                    FMHEDC
     C                   KFLD                    FMQUE#
     C                   KFLD                    FMANS#
      *
      *          ------------------------------------------
      *          Retrieve authorization
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
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
     C                   WHEN      E#FILE = 'OMH030D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMAZHL1 '                          X02
     C                   MOVEL     AZHL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMAZDL1 '                          X02
     C                   MOVEL     AZDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHQUL1 '                          X02
     C                   MOVEL     HQUL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHQLL1 '                          X02
     C                   MOVEL     HQLL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMANSL1 '                          X02
     C                   MOVEL     ANSL1         P#FILE
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
      *          Else exit immediately.
      *          ------------------------------------------
     C                   ELSE                                                   X01
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit.
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
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
      * It is called one time per program execution.                 *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Subroutine init.
      *          ------------------------------------------
     C                   EVAL      %SUBST(MSGF:1:6) = 'OMHMSG'
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 20                                 B01
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
**
PMT0006 - 0=*ANY, F4=List            1
PMT0005 - F4=List                    2
QUE0001 - question                   3
