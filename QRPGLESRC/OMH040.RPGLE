     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH040                                        *
      * Function ...: Help desk maintenance: mail extensions        *
      * Module .....: Help desk                                     *
      *                                                             *
      * Author .....: Remain Software                               *
      * Action .....: Creation.                                     *
      *                                                             *
      * Parameters .: P#HEDC P#STAT                                 *
      *                                                             *
      * This program maintains the help desk file                   *
      *                                                             *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators                                             *
      * ----------------------------------------------------------- *
      *                                                             *
      * 70-76 - Screen field error indicators                       *
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
     FOMTEML1   UF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(TEML1)
     FOMHDDL1   IF   E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(HDDL1)
     FOMMHXL1   UF A E           K DISK
     F                                     INFSR(*PSSR)
     F                                     INFDS(MHXL1)
     FOMH040D   CF   E             WORKSTN INFDS(INFO)
     F                                     INFSR(*PSSR)
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      *                                                              *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * MC    Used to fill message variables in message data parm.   *
      * ------------------------------------------------------------ *
     D VAR             S            256    DIM(10)
     D MSD             S              7    DIM(10) CTDATA PERRCD(1)
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
     D HDDL1           DS            57
     D MHXL1           DS            57
     D TEML1           DS            57
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
     D C#SDFT          C                   CONST('*SYSDFT   ')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#EXIT          C                   CONST('*EXIT')
     D C#CNCL          C                   CONST('*CNCL')
     D C#FILE          C                   CONST('*FILE      ')
     D C#SFL1          C                   CONST(11)
      * Prototype for OMH040
     D OMH040          PR
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
      * Prototype for 'OMH042'
     D OMH042          PR                  EXTPGM('OMH042')
     D P3HEDC_                             LIKE(P3HEDC)
     D P3CUSC_                             LIKE(P3CUSC)
     D P3CCTC_                             LIKE(P3CCTC)
     D P3ACTC_                             LIKE(P3ACTC)
     D P3MTDI_                             LIKE(P3MTDI)
     D P3STAT_                             LIKE(P3STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH901C (send message)
      * Prototype for 'OMH905C'
     D OMH905C         PR                  EXTPGM('OMH905C')
     D P#USID_                             LIKE(P#USID)
     D P#USCL_                             LIKE(P#USCL)
     D P#GRID_                             LIKE(P#GRID)
     D P#GRCL_                             LIKE(P#GRCL)
     D P#USDC_                             LIKE(P#USDC)
     D P2STAT_                             LIKE(P2STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH909C (Check object)
      * Prototype for 'OMH909C'
     D OMH909C         PR                  EXTPGM('OMH909C')
     D P#OBJL_                             LIKE(P#OBJL)
     D P#OBJC_                             LIKE(P#OBJC)
     D P#MATM_                             LIKE(P#MATM)
     D P#OBJT_                             LIKE(P#OBJT)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMTEML1 Template File
      * Prototype for 'OMH029'
     D OMH029          PR                  EXTPGM('OMH029')
     D P#OPTI_                             LIKE(P#OPTI)
     D P1HEDC_                             LIKE(P1HEDC)
     D P#SUL#_                             LIKE(P#SUL#)
     D P1STAT_                             LIKE(P1STAT)
     D D#D7HD_                             LIKE(D#D7HD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH042
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
     D OMH040          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D EOF             S                   LIKE(INDI)
     D EROR01          S                   LIKE(INDI)
     D I#PSSR          S              1
     D K#ACTC          S                   LIKE(ACTC)
     D K#CCTC          S                   LIKE(CCTC)
     D K#CUSC          S                   LIKE(CUSC)
     D K#HEDC          S                   LIKE(HEDC)
     D LOOP01          S                   LIKE(INDI)
     D MA_X            S              5  0
     D MS              S              5  0
     D MSGDTA          S            256
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#FILE          S             57
     D P#GRCL          S             10
     D P#GRID          S             10
     D P#INKC          S                   LIKE(INDI)
     D P#MATM          S                   LIKE(MATM)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#OBJC          S                   LIKE(OBJC)
     D P#OBJL          S                   LIKE(OBJL)
     D P#OBJT          S                   LIKE(OBJT)
     D P#OPTI          S                   LIKE(INDI)
     D P#PMQC          S             10
     D P#SUL#          S                   LIKE(SUL#)
     D P#USCL          S             10
     D P#USDC          S             50
     D P#USID          S             10
     D P1HEDC          S                   LIKE(HEDC)
     D P1STAT          S                   LIKE(STAT)
     D P2STAT          S                   LIKE(STAT)
     D P3ACTC          S                   LIKE(ACTC)
     D P3CCTC          S                   LIKE(CCTC)
     D P3CUSC          S                   LIKE(CUSC)
     D P3HEDC          S                   LIKE(HEDC)
     D P3MTDI          S                   LIKE(INDI)
     D P3STAT          S                   LIKE(STAT)
     D SEL             S                   LIKE(INDI)
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
      *          Parameterlist
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Work with Application Relations.
      *          ------------------------------------------
     C                   IF        P#STAT = C#NORM                              B01
     C                   EXSR      SRSC01
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Exit
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
     C                   FOR       MA_X = 1 TO %ELEM(MC)
     C                   EVAL      MC(MA_X) = *BLANK
     C                   ENDFOR
     C                   EVAL      MSGDTA = *BLANK
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
      * This routine displays the second screen. The change, add    *
      * and delete options, are combined into one screen, controled *
      * with indicator 80 & 81.                                     *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
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
      *          Display the screen
      *          ------------------------------------------
     C                   EXFMT     OMH04004
     C                   IF        *INKC = *ON
     C                   EXSR      SRTTLR
     C                   ENDIF
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
     C                   EVAL      *IN(78) = *OFF
     C                   EVAL      *IN(79) = *OFF
     C                   EVAL      EROR01 = *OFF
     C                   EVAL      LOOP01 = *OFF
      *
      *          ------------------------------------------
      *          Command key processing.
      *          F5=Refresh
      *          ------------------------------------------
     C                   IF        *INKE = *ON                                  B03
     C                   EXSR      SRRF01
     C                   EVAL      EROR01 = *ON
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Roll up: templates
      *          ------------------------------------------
     C                   IF        *IN25 = *ON                                  B04
     C                   EVAL      P3HEDC = P#HEDC
     C                   EVAL      P3CUSC = *BLANKS
     C                   EVAL      P3CCTC = *BLANKS
     C                   EVAL      P3MTDI = '0'
     C                   IF        *IN80 = *OFF
     C                             AND *IN81 = *OFF
     C                   EVAL      P3ACTC = '60'
     C                   ENDIF
     C                   IF        *IN80 = *ON
     C                             AND *IN81 = *OFF
     C                   EVAL      P3ACTC = '20'
     C                   ENDIF
     C                   IF        *IN80 = *ON
     C                             AND *IN81 = *ON
     C                   EVAL      P3ACTC = '40'
     C                   ENDIF
     C                   EVAL      P3STAT = C#NORM
     C                   CALLP     OMH042 ( P3HEDC : P3CUSC : P3CCTC :
     C                             P3ACTC : P3MTDI : P3STAT )
     C                   EVAL      *IN25 = *OFF
     C                   EVAL      EROR01 = *ON
     C                   IF        *IN80 = *ON
     C                             AND *IN81 = *ON
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   ENDIF
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Do edit routine if not F12.
      *          ------------------------------------------
     C                   IF        *INKL = *OFF                                 B05
     C                   EXSR      SRED01
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
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
      * This routine blanks the input fields on the screen.         *
      * Pending on the required action. The change, add and delete  *
      * options are combined into one screen, controlled by indica- *
      * tor 80 and 81.                                              *
      *                                                             *
      * 80 off & 81 off = Add                                       *
      * 80 on  & 81 off = Change                                    *
      * 80 on  & 81 on  = Delete                                    *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRIN01        BEGSR
      *
      *          ------------------------------------------
      *          Do refresh routine
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
      * This screen is called for future processing.                *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRRF01        BEGSR
      *
      *                    --------------------------------
      *                    Check Helpdesk
      *                    --------------------------------
     C     P#HEDC        CHAIN     OMHDDL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   EVAL      P#STAT = C#TERM
     C                   EXSR      SRTTLR
     C                   ELSE                                                   X01
     C                   MOVEL     DDHEDC        FMHEDC
     C                   MOVEL     DDHEDD        FMHEDD
      *
      *          ------------------------------------------
      *          Determine the action
      *          ------------------------------------------
     C     P#HEDC        CHAIN(N)  OMMHXL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON
     C                   EVAL      *IN(80) = *OFF
     C                   EVAL      *IN(81) = *OFF
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   EVAL      *IN(80) = *ON
     C                   EVAL      *IN(81) = *OFF
     C                   ENDIF
      *
      *                    --------------------------------
      *                    Add: clear the fields
      *                    --------------------------------
     C     P#HEDC        CHAIN(N)  OMMHXL1
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      *IN80 = *OFF                                 B01
     C                   EVAL      *IN81 = *OFF                                 B01
     C                   MOVE      *BLANKS       FMAPII
     C                   MOVEL     *BLANKS       FMSWTU
     C                   MOVEL     *BLANKS       FMHUID
     C                   MOVEL     *BLANKS       FMHADR
     C                   MOVEL     *BLANKS       FMEMAC
     C                   MOVEL     *BLANKS       FMEXTI
     C                   MOVEL     *BLANKS       FMEEMC
     C                   MOVEL     *BLANKS       FMMATF
     C                   MOVEL     *BLANKS       FMMATL
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Get the file if no addition
      *          ------------------------------------------
     C                   MOVEL     HXAPII        FMAPII
     C                   MOVEL     HXSWTU        FMSWTU
     C                   MOVEL     HXHUID        FMHUID
     C                   MOVEL     HXHADR        FMHADR
     C                   MOVEL     HXEMAC        FMEMAC
     C                   MOVEL     HXEXTI        FMEXTI
     C                   MOVEL     HXEEMC        FMEEMC
     C                   MOVEL     HXMATF        FMMATF
     C                   MOVEL     HXMATL        FMMATL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Set to system default if the template is blanks.
      *          ------------------------------------------
     C                   IF        FMMATF = *BLANKS                             B01
     C                   MOVE      C#SDFT        FMMATF
     C                   MOVE      C#SDFT        FMMATL
     C                   ENDIF                                                  E01
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
      * This routine checks if the several Function attributes are  *
      * filled.                                                     *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRED01        BEGSR
      *
      *          ------------------------------------------
      *          If not deletion
      *          ------------------------------------------
     C                   IF        *IN81 = *OFF                                 B01
      *
      *          ------------------------------------------
      *          Test if deletion.
      *          ------------------------------------------
     C                   IF        *IN23 = *ON                                  B02
     C                   EVAL      MSGID = 'DLT0001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN80 = *ON
     C                   EVAL      *IN81 = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Auto process code must be 0 or 1
      *          ------------------------------------------
     C                   EVAL      *IN81 = *OFF
     C                   IF        FMAPII <> '0'                                B03
     C                             AND FMAPII <> '1'
     C                   EVAL      MSGDTA = VAR(1)
     C                   EVAL      MSGID = 'IND0015'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN72 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if the user id. may be blanks
      *          ------------------------------------------
     C                   IF        FMAPII = '1'                                 B03
     C                   IF        FMSWTU = *BLANKS                             B04
     C                   EVAL      MSGDTA = VAR(3)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EVAL      *IN73 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ELSE                                                   X04
      *
      *          ------------------------------------------
      *          Check if the user id. exists
      *          ------------------------------------------
     C                   EVAL      P2STAT = *BLANKS
     C                   MOVEL     FMSWTU        P#USID
     C                   CALLP     OMH905C ( P#USID : P#USCL : P#GRID :
     C                             P#GRCL : P#USDC : P2STAT )
     C                   IF        P2STAT <> C#NORM                             B05
     C                   EVAL      MSGID = 'USRE003'
     C                   EVAL      *IN73 = *ON
     C                   EVAL      EROR01 = *ON
     C                   EXSR      SNDMSG
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if helpdesk user is filled
      *          ------------------------------------------
     C                   IF        FMHUID = *BLANKS                             B03
     C                   EVAL      MSGDTA = VAR(5)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN74 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Adress may not be blank
      *          ------------------------------------------
     C                   IF        FMHADR = *BLANKS                             B03
     C                   EVAL      MSGDTA = VAR(6)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN75 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          E-mail address may not be blanks
      *          ------------------------------------------
     C                   IF        FMEMAC = *BLANKS                             B03
     C                   EVAL      MSGDTA = VAR(9)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Mail template location may not be blanks
      *          ------------------------------------------
     C                   IF        FMMATF = *BLANKS                             B03
     C                             OR FMMATF = C#SDFT                           B03
     C                   MOVEL     C#SDFT        FMMATF
     C                   MOVEL     C#SDFT        FMMATL
     C                   ELSE                                                   X03
      *
     C                   IF        FMMATL = *BLANKS                             B04
     C                   EVAL      MSGDTA = VAR(8)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN79 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Mail template location must exist
      *          ------------------------------------------
     C                   IF        FMMATF <> C#SDFT                             B03
      *
     C                   MOVE      FMMATL        P#OBJL
     C                   MOVEL(P)  FMMATF        P#OBJC
     C                   EVAL      P#MATM = *BLANKS
     C                   EVAL      P#OBJT = C#FILE
     C                   EVAL      P#STAT = C#NORM
     C                   CALLP     OMH909C ( P#OBJL : P#OBJC : P#MATM :
     C                             P#OBJT : P#STAT )
     C                   IF        P#STAT = C#TERM                              B04
     C                   EVAL      MSGDTA = VAR(7)
     C                   EVAL      MSGID = 'EXI0010'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN78 = *ON
     C                   EVAL      *IN79 = *ON
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Check if indicator has the right value
      *          ------------------------------------------
     C                   IF        FMEXTI <> '0'                                B03
     C                             AND FMEXTI <> '1'
     C                             AND FMEXTI <> '2'
     C                   EVAL      MSGDTA = VAR(2)
     C                   EVAL      MSGID = 'IND0015'
     C                   EXSR      SNDMSG
     C                   EVAL      *IN76 = *ON
     C                   EVAL      EROR01 = *ON
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Mist the e-mail adress be there?
      *          ------------------------------------------
     C                   IF        FMEXTI <> '0'                                B03
     C                             AND FMEEMC = *BLANKS                         B01
     C                   EVAL      MSGDTA = VAR(4)
     C                   EVAL      MSGID = 'BLAE001'
     C                   EXSR      SNDMSG
     C                   EVAL      EROR01 = *ON
     C                   EVAL      *IN77 = *ON
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * SRPR01 - Process screen 1                                   *
      *                                                             *
      * This routine updates the application file                   *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRPR01        BEGSR
      *
      *          ------------------------------------------
      *          Reloop if fields where changed.
      *          ------------------------------------------
     C                   MOVE      *IN40         LOOP01
      *
      *          ------------------------------------------
      *          Delete the file if wanted.
      *          ------------------------------------------
     C                   IF        *IN80 = *ON                                  B01
     C                             AND *IN81 = *ON
     C     $MHXK1        DELETE    OMMHXR
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Delete dependent templates
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        K#HEDC
     C                   EVAL      K#CUSC = *BLANKS
     C                   EVAL      K#CCTC = *BLANKS
     C                   MOVEL     *LOVAL        K#ACTC
     C     $EM1K4        SETLL     OMTEMR
     C     $EM1K3        READE     OMTEMR
     C                   EVAL      *IN99 = %EOF
     C                   DOW       *IN99 <> *ON                                 B02
     C                   DELETE    OMTEMR
     C     $EM1K3        READE     OMTEMR
     C                   EVAL      *IN99 = %EOF
     C                   ENDDO                                                  E02
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Do only if fields where changed.
      *          ------------------------------------------
     C                   IF        *IN40 = *ON                                  B02
      *
      *          ------------------------------------------
      *          Get the file
      *          ------------------------------------------
     C     $MHXK1        CHAIN     OMMHXL1
     C                   EVAL      *IN99 = NOT %FOUND
      *
      *          ------------------------------------------
      *          Fill the file
      *          ------------------------------------------
     C                   MOVEL     FMHEDC        HXHEDC
     C                   MOVEL     FMAPII        HXAPII
     C                   MOVEL     FMSWTU        HXSWTU
     C                   MOVEL     FMHUID        HXHUID
     C                   MOVEL     FMHADR        HXHADR
     C                   MOVEL     FMEMAC        HXEMAC
     C                   MOVEL     FMEXTI        HXEXTI
     C                   MOVEL     FMEEMC        HXEEMC
      *
     C                   IF        FMMATF = C#SDFT                              B03
     C                   MOVE      *BLANKS       HXMATF
     C                   MOVE      *BLANKS       HXMATL
     C                   ELSE                                                   X03
     C                   MOVEL     FMMATF        HXMATF
     C                   MOVEL     FMMATL        HXMATL
     C                   ENDIF                                                  E03
      *
      *          ------------------------------------------
      *          Add or update the file.
      *          ------------------------------------------
     C                   IF        *IN99 = *OFF
     C                   UPDATE    OMMHXR
     C                   ENDIF
     C                   IF        *IN99 = *ON
     C                   WRITE     OMMHXR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Force UPDATE mode if in ADD mode.
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
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMX001 (Get system info)
      *          ------------------------------------------
     C     $MH029        PLIST
     C                   PARM                    P#OPTI
     C                   PARM                    P1HEDC
     C                   PARM                    P#SUL#
     C                   PARM                    P1STAT
     C                   PARM                    D#D7HD
      *
      *          ------------------------------------------
      *          Parameter list for OMH042
      *          ------------------------------------------
     C     $MH042        PLIST
     C                   PARM                    P3HEDC
     C                   PARM                    P3CUSC
     C                   PARM                    P3CCTC
     C                   PARM                    P3ACTC
     C                   PARM                    P3MTDI
     C                   PARM                    P3STAT
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
     C                   PARM                    P2STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH909C (Check object)
      *          ------------------------------------------
     C     $X909C        PLIST
     C                   PARM                    P#OBJL
     C                   PARM                    P#OBJC
     C                   PARM                    P#MATM
     C                   PARM                    P#OBJT
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          OMTEML1 Template File
      *          ------------------------------------------
     C     $EM1K3        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
     C     $EM1K4        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
     C                   KFLD                    K#ACTC
      *
      *          ------------------------------------------
      *          OMMHXL1 Help Desk File
      *          ------------------------------------------
     C     $MHXK1        KLIST
     C                   KFLD                    FMHEDC
      *
      *          ------------------------------------------
      *          Retrieve authorisation
      *          ------------------------------------------
     C                   EVAL      P#OPTI = '2'
     C                   EVAL      P#SUL# = '9'
     C                   EVAL      P1HEDC = *BLANKS
     C                   EVAL      P1STAT = *BLANKS
     C                   CALLP     OMH029 ( P#OPTI : P1HEDC : P#SUL# :
     C                             P1STAT : D#D7HD )
      *
      *          ------------------------------------------
      *          Continue if authorisation ok
      *          ------------------------------------------
     C                   IF        P1STAT = C#NORM                              B01
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
      * SRTTLR - Total last record processing                       *
      *                                                             *
      * This routine sets the last record indicator on.             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRTTLR        BEGSR
      *
      *          ------------------------------------------
      *          Cancel, quit or normal
      *          ------------------------------------------
     C                   IF        *INKC = *ON
     C                   EVAL      P#STAT = C#EXIT
     C                   ENDIF
     C                   IF        *INKL = *ON
     C                   EVAL      P#STAT = C#CNCL
     C                   ENDIF
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
      * It is called one time per program execution. If the system   *
      * choose to do so, it can be paged out. Thats why it is here.  *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRGVAR        BEGSR
      *
      *          ------------------------------------------
      *          Get all message id texts
      *          ------------------------------------------
     C                   FOR       MS = 1 TO 10                                 B01
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
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMH001D '                          X02
     C                   MOVEL     INFO          P#FILE
      *
     C                   WHEN      E#FILE = 'OMMHXL1 '                          X02
     C                   MOVEL     MHXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMHDDL1 '                          X02
     C                   MOVEL     HDDL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMTEML1 '                          X02
     C                   MOVEL     TEML1         P#FILE
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
**
OPT0006  -  '0', '1'                          1
VAL0002  -  '0', '1', '2'                     2
USR0003  -  User ID                           3
EMA0001  -  Email adress                      4
HLP0015  -  User                              5
HLP0016  -  Adress                            6
FIL0005  -  file                              7
LIB0001  -  library                           8
EMA0001  -  E-mail address                    9
