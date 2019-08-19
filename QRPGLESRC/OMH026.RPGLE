     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH026                                        *
      * Function ...: Select incidents for escalation               *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#STAT - Status                               *
      *                                                             *
      * Modification: 19-8-1997                                     *
      * Author .....: Remain Software                               *
      *               be activated at entrance or/and when leaving  *
      *               a given priority range, or always when an     *
      *               escalation takes place. This is handled by    *
      *               the subroutines SRPGME (entry-exit), SRPGMA   *
      *               (all) and SRRUN (call program after check).   *
      *                                                             *
      *               If next priority range is not met, we search  *
      *               till 999                                      *
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Used indicators.  0=false  1=true                           *
      * ----------------------------------------------------------- *
      *                                                             *
      * 99 - General purpose indicator.                             *
      *                                                             *
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMINCL3   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(INC)
     FOMEPRL1   UF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(EPR)
     FOMESCL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(ESC)
     FOMH026P   O    E             PRINTER INFSR(*PSSR)
     F                                     OFLIND(*IN01)
     F                                     INFDS(MH026)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Array definitions                                           *
      * ----------------------------------------------------------- *
      *
      * VAR   Contains variable message text                         *
      * MSD   Contains variable message id (compile time array)      *
      * ------------------------------------------------------------ *
     D VAR             S            130    DIM(5)
     D MSD             S              7    DIM(5) CTDATA PERRCD(1)
      *
     D HDC             S              5    DIM(9999)
     D SUL             S              1    DIM(9999)
     D STS             S              5    DIM(9999)
     D PRL             S              3  0 DIM(9999)
     D PRH             S              3  0 DIM(9999)
     D DYS             S              3  0 DIM(9999)
     D HRS             S              5  2 DIM(9999)
     D PIN             S              3  0 DIM(9999)
     D PRG             S             10    DIM(9999)
     D LIB             S             10    DIM(9999)
     D EOE             S              1    DIM(9999)
      *COPY QCPYSRC,OMSREFREN
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Input specifications                                        *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  E#FILE               201    208
     D  D#JOBN               244    253
     D  D#USID               254    263
     D  D#JOB#               264    269  0
      *
      *          ------------------------------------------
      *          Information structures
      *          ------------------------------------------
     D EPR             DS            57
     D ESC             DS            57
     D INC             DS            57
     D MH026           DS            57
      *
      *          ------------------------------------------
      *          Priority workfield
      *          ------------------------------------------
     D                 DS                  INZ
     D D#PRI#                         4  0
      *
      *          ------------------------------------------
      *          Date convertor.
      *          ------------------------------------------
     D                 DS
     D D#DATA                         7
     D  D#DATE                        7  0 OVERLAY(D#DATA)
     D   D#CC                         1  0 OVERLAY(D#DATE)
     D   D#YY                         2  0 OVERLAY(D#DATE:2)
     D   D#MM                         2  0 OVERLAY(D#DATE:4)
     D   D#DD                         2  0 OVERLAY(D#DATE:6)
      *
      *          ------------------------------------------
      *          Field containing time
      *          ------------------------------------------
     D                 DS                  INZ
     D D#THEN                         6  0
     D  D#THHR                        2  0 OVERLAY(D#THEN)
     D  D#THMN                        2  0 OVERLAY(D#THEN:3)
     D  D#THSC                        2  0 OVERLAY(D#THEN:5)
     D D#NOW                          6  0
     D  D#NWHR                        2  0 OVERLAY(D#NOW)
     D  D#NWMN                        2  0 OVERLAY(D#NOW:3)
     D  D#NWSC                        2  0 OVERLAY(D#NOW:5)
     D D#CNT#                         6  0
     D D#CNN#                         6  0
     D D#HLP#                         5  0
     D D#DAYT                         2  0
     D D#DAYN                         2  0
      *
      *          ------------------------------------------
      *          Field used for hour counting
      *          ------------------------------------------
     D                 DS                  INZ
     D D#EPTM                         6  0
     D  D#EPMN                        2  0 OVERLAY(D#EPTM:3)
     D D#SPTM                         6  0
     D  D#SPMN                        2  0 OVERLAY(D#SPTM:3)
     D D#HRS#                         4  2
     D D#HEF#                         6  2
     D D#HDF#                        10  2
     D D#WRK#                         6  0
     D D#TIME                         6  0
     D  D#MINU                        2  0 OVERLAY(D#TIME:3)
      *
      *          ------------------------------------------
      *          Copy member for OMQCHGIN - Change incident
      *          ------------------------------------------
      /COPY QCPYLESRC,OMQCHGINDS
      *
      *          ------------------------------------------
      *          The field reference file is used for field definitions
      *          ------------------------------------------
     D               E DS                  EXTNAME(OMSREFHD)
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
      *          Command keyfields
      *          ------------------------------------------
     D                 DS
     D  CMD                    1     20
     D                                     DIM(1) CTDATA PERRCD(1)
     D  W#SEC#                12     19  0
      *
      *          ------------------------------------------
      *          Workfields
      *          ------------------------------------------
     D                 DS
     D D#PGMD                        20
      *
      *          ------------------------------------------
      *          Named constants for status field
      *          ------------------------------------------
     D C#INTL          C                   CONST('V3R0M0')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#PGM           C                   CONST('*PGM     ')
     D C#LIBL          C                   CONST('*LIBL     ')
      * Prototype for OMH026
     D OMH026          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN        PR                  EXTPGM('OMQCHGIN')
     D I4INTL_                             LIKE(I4INTL)
     D I4ACTC_                             LIKE(I4ACTC)
     D I4BUF_                              LIKE(I4BUF)
     D I4PRC_                              LIKE(I4PRC)
     D I4ERR_                              LIKE(I4ERR)
     D I4FLD_                              LIKE(I4FLD)
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      * Prototype for 'OMQCHGIN'
     D OMQCHGIN_001    PR                  EXTPGM('OMQCHGIN')
      * Prototype for 'QCMDEXC'
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D P#CMD_                              LIKE(P#CMD)
     D P#CMD#_                             LIKE(P#CMD#)
      *
      *          ------------------------------------------
      *          Parameter list for OMX931
      * Prototype for 'OMX931'
     D OMX931          PR                  EXTPGM('OMX931')
     D P#DATF_                             LIKE(P#DATF)
     D P#TIMF_                             LIKE(P#TIMF)
     D P#DATT_                             LIKE(P#DATT)
     D P#TIMT_                             LIKE(P#TIMT)
     D P#SPTM_                             LIKE(P#SPTM)
     D P#EPTM_                             LIKE(P#EPTM)
     D P#DOWA_                             LIKE(P#DOWA)
     D P#HDF#_                             LIKE(P#HDF#)
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGIN
      * Prototype for 'OMH906C'
     D OMH906C         PR                  EXTPGM('OMH906C')
     D P2PGMN_                             LIKE(P2PGMN)
     D P2LIBC_                             LIKE(P2LIBC)
     D P2OBJT_                             LIKE(P2OBJT)
     D P2OBJD_                             LIKE(P2OBJD)
     D P2STAT_                             LIKE(P2STAT)
      *
      *          ------------------------------------------
      *          Set status
      * Prototype for D#PGMD
     D D#PGMD_001      PR                  EXTPGM(D#PGMD)
     D P1HEDC_                             LIKE(P1HEDC)
     D P1INC#_                             LIKE(P1INC#)
     D P1PRIO_                             LIKE(P1PRIO)
     D P1PRIN_                             LIKE(P1PRIN)
     D P1STAT_                             LIKE(P1STAT)
      *
      *          ------------------------------------------
      *          Parameter list for OMH906C (Check object)
      * Prototype for 'OMH904C'
     D OMH904C         PR                  EXTPGM('OMH904C')
     D MSGID_                              LIKE(MSGID)
     D MSGF_                               LIKE(MSGF)
     D MSGLIB_                             LIKE(MSGLIB)
     D MSGTXT_                             LIKE(MSGTXT)
      *
      *          ------------------------------------------
      *          Parameter list for entry/exit program
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
     D OMH026          PI
     D P#HEDC                         5
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D k#pri#          S              3P 0
     D k#prl#          S              3P 0
     D p#eptm          S              6P 0
     D p#sptm          S              6P 0
     D p#timf          S              6P 0
     D p#timt          S              6P 0
     D p1prin          S              3P 0
     D p1prio          S              3P 0
     D DO_X            S              7  0
     D I#PSSR          S              1
     D IX              S              7  0
     D DTA#            S              7
     D IY              S              4  0
     D IZ              S              4  0
     D K#HEDC          S                   LIKE(HEDC)
     D K#INC#          S                   LIKE(INC#)
     D K#STAT          S                   LIKE(STAT)
     D K#SUL#          S                   LIKE(SUL#)
     D MS              S              5  0
     D MSGGET          S              1
     D MSGID           S              7
     D MSGLIB          S             10
     D MSGTXT          S            132
     D P#CMD           S             20
     D P#CMD#          S             15  5
     D P#CTN#          S                   LIKE(CTN#)
     D P#DATF          S                   LIKE(DTA#)
     D P#DATT          S                   LIKE(DTA#)
     D P#DOWA          S                   LIKE(DOWA)
     D P#FILE          S             57
     D P#HDF#          S             10  2
     D P#INC#          S                   LIKE(INC#)
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#PMQC          S             10
     D P#SUL#          S                   LIKE(SUL#)
     D P1HEDC          S                   LIKE(HEDC)
     D P1INC#          S                   LIKE(INC#)
     D P1STAT          S                   LIKE(STAT)
     D P2LIBC          S                   LIKE(LIBC)
     D P2OBJD          S             50
     D P2OBJT          S              9
     D P2PGMN          S                   LIKE(PGMN)
     D P2STAT          S                   LIKE(STAT)
     D W#EOFI          S                   LIKE(INDI)
     D W#ESCL          S                   LIKE(INDI)
     D W#SKPI          S                   LIKE(INDI)
     D W#VALI          S                   LIKE(INDI)
     D W#WKH#          S              5  0
     D W#WK5#          S              5  0
     D X1              S              4  0
      *----------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *----------------------------------------------------------------------
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      *          «««  mainline  »»»                                 *
      * ----------------------------------------------------------- *
      *
      *          ------------------------------------------
      *          Program parameters
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Exit if no parameters passed
      *          ------------------------------------------
     C                   IF        D#PARM = *ZEROS                              B01
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Process as long as required
      *          ------------------------------------------
     C                   DOW       W#VALI = *ON                                 B01
     C                   EXSR      SRVERW
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - PTFCIEDTng                                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          What time is it?
      *          ------------------------------------------
     C                   EVAL      D#THEN = %DEC(%TIME)
     C                   EVAL      D#DAYT = *DAY
      *
      *          ------------------------------------------
      *          Initialize work- & keyfields
      *          ------------------------------------------
     C                   EVAL      IZ = 1
     C                   DOW       HDC(IZ) <> *BLANKS                           B01
     C                   EVAL      W#EOFI = *OFF
     C                   EVAL      K#HEDC = HDC(IZ)
     C                   EVAL      K#SUL# = SUL(IZ)
     C                   EVAL      K#STAT = STS(IZ)
     C                   MOVE      *LOVAL        K#PRI#
     C     $NC3K2        SETLL     OMINCR
      *
      *          ------------------------------------------
      *          Read all incidents from this HelpDesk
      *          ------------------------------------------
     C                   DOW       W#EOFI <> *ON                                B02
     C     $NC3K1        READE     OMINCR
     C                   EVAL      *IN99 = %EOF
     C                   IF        *IN99 = *ON                                  B03
     C                   EVAL      W#EOFI = *IN99
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Fill work- and printfields
      *          ------------------------------------------
     C                   Z-ADD     NCPRI#        W#WK5#
     C                   Z-ADD     NCPRI#        W#WKH#
     C                   Z-ADD     NCESC#        PRESC#
      *
      *          ------------------------------------------
      *          Check whether escalation is required
      *          ------------------------------------------
     C                   EXSR      SRESCL
      *
      *          ------------------------------------------
      *          Apparently it is.
      *          ------------------------------------------
     C                   IF        W#ESCL = *ON                                 B04
      *
      *          ------------------------------------------
      *          Fill fields in the buffer
      *          ------------------------------------------
     C                   CLEAR                   I4BUF
      *
     C                   MOVEL     C#INTL        I4INTL                         Interface level
     C                   MOVE      '33'          I4ACTC                         "Escalate"
     C                   MOVEL     NCHEDC        I4HEDC                         HelpDesk
     C                   MOVEL     NCINC#        I4INC#                         Incident
     C                   Z-ADD     NCESC#        I4ESC#
      *
     C                   IF        W#WK5# > 999                                 B05
     C                   EVAL      W#WK5# = 999
     C                   ENDIF                                                  E05
     C                   Z-ADD     W#WK5#        I4PRI#                         New priority
     C                   MOVE      '3'           I4PCM                          Absolute value
     C                   MOVE      '3'           I4CCM                          Absolute value
     C                   Z-ADD     888888        I4IEDT                         Update time and date
     C                   Z-ADD     D#SPTM        I4IETM                          D#SPTM: see SRDTTM
      *
     C                   MOVE      '1'           I4UPD
     C                   MOVE      '1'           I4UPB
     C                   MOVE      '0'           I4EOP
      *
      *          ------------------------------------------
      *          Call OMQCHGIN
      *          ------------------------------------------
     C                   EVAL      I4INTL = C#INTL
     C                   EVAL      I4ACTC = '33'
     C                   CALLP(E)  OMQCHGIN ( I4INTL : I4ACTC : I4BUF :
     C                             I4PRC : I4ERR : I4FLD )
     C                   EVAL      *IN99 = %ERROR
      *          ------------------------------------------
      *          The xxIEDT fields in the printerfile
      *          have only 6 digits, no century.
      *          ------------------------------------------
     C                   Z-ADD     NCIEDT        FCIEDT
     C                   Z-ADD     I4IEDT        F4IEDT
      *          ------------------------------------------
      *          Print a line on the report.
      *          ------------------------------------------
     C                   MOVEL     NCSHID        FMSHID
     C                   EXSR      SRPRNT
      *
      *          ------------------------------------------
      *          If OMQCHGIN ended with an error, close the API
      *          and do next incident.
      *          ------------------------------------------
     C                   IF        *IN99 = *ON                                  B05
     C                             OR I4MSTP = 'E'
     C                   CALLP     OMQCHGIN_001()
     C                   ENDIF                                                  E05
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          Next set of array elements
      *          ------------------------------------------
     C                   ADD       1             IZ
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Continue?
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMEPRR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   EVAL      W#VALI = *OFF
     C                   ELSE                                                   X01
     C                   IF        PRSAMI = *OFF                                B02
     C                   EVAL      W#VALI = *OFF
     C                   EVAL      PRJOB# = 0
     C                   MOVE      *BLANKS       PRJOBN
     C                   MOVE      *BLANKS       PRUSID
     C                   UPDATE    OMEPRR
     C                   ELSE                                                   X02
     C                   UPDATE    OMEPRR
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If so, wait till the next run
      *          ------------------------------------------
     C                   IF        W#VALI = *ON                                 B01
      *
      *          ------------------------------------------
      *          What time is it, and how many time this job has taken?
      *          (express timestamps in seconds, evaluate)
      *          ------------------------------------------
     C                   EVAL      D#NOW = %DEC(%TIME)
     C                   EVAL      D#DAYN = *DAY
     C                   EVAL      D#CNT# = D#THHR * 3600
     C                   EVAL      D#HLP# = D#THMN * 60
     C                   ADD       D#HLP#        D#CNT#
     C                   ADD       D#THSC        D#CNT#
     C                   EVAL      D#CNN# = D#NWHR * 3600
     C                   EVAL      D#HLP# = D#NWMN * 60
     C                   ADD       D#HLP#        D#CNN#
     C                   ADD       D#NWSC        D#CNN#
     C                   IF        D#NOW <= D#THEN                              B02
     C                   ADD       86400         D#CNN#
     C                   ELSE                                                   X02
     C                   IF        D#NOW = D#THEN                               B03
     C                             AND D#DAYN <> D#DAYT
     C                   ADD       86400         D#CNN#
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C     D#CNN#        SUB       D#CNT#        D#HLP#
     C                   DIV       15            D#HLP#
      *
      *          ------------------------------------------
      *          Calculate waiting time, and wait
      *          ------------------------------------------
     C     240           MULT      PRESF#        IX
     C                   SUB       D#HLP#        IX
     C                   EVAL      W#SEC# = 15
     C                   EVAL      P#CMD = CMD(1)
     C                   IF        IX > 0                                       B02
     C                   FOR       DO_X = 1 TO IX                               B03
     C                   EVAL      P#CMD# = 20
     C                   CALLP     QCMDEXC ( P#CMD : P#CMD# )
      *
      *          ------------------------------------------
      *          End request?
      *          ------------------------------------------
     C                   EXSR      SRDOWN
     C                   ENDFOR                                                 E03
     C                   ELSE                                                   X02
     C                   EXSR      SRDOWN
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Continue?
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMEPRR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B02
     C                   EVAL      W#VALI = *OFF
     C                   ELSE                                                   X02
     C                   IF        PRSAMI = *OFF                                B03
     C                   EVAL      W#VALI = *OFF
     C                   EVAL      PRJOB# = 0
     C                   MOVE      *BLANKS       PRJOBN
     C                   MOVE      *BLANKS       PRUSID
     C                   UPDATE    OMEPRR
     C                   ELSE                                                   X03
     C                   UPDATE    OMEPRR
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
      * ------------------------------------------------------------ *
      * SRESCL - Check whether escalation is required                * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRESCL        BEGSR
      *
      *          ------------------------------------------
      *          Process
      *          ------------------------------------------
     C                   EVAL      W#ESCL = *OFF
      *
      *          ------------------------------------------
      *          Can we continue for this priority?
      *          ------------------------------------------
     C                   IF        W#WK5# >= PRL(IZ)                            B01
     C                             AND W#WK5# <= PRH(IZ)
      *
      *          ------------------------------------------
      *          Can we continue for this dates?
      *          ------------------------------------------
     C                   IF        NCIEDT <> 0                                  B02
     C                             OR NCSRDT <> 0
      *
      *          ------------------------------------------
      *          Is it already time to escalate?
      *          ------------------------------------------
     C                   EXSR      SRDTTM
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
      * SRDTTM - Calculate escalation moment                        *
      *                                                             *
      * This routine is executed to check if escalation is needed.  *
      * The following rules apply:                                  *
      *                                                             *
      * ----------------------------------------------------------- *
     C     SRDTTM        BEGSR
      *
      *          ------------------------------------------
      *          Count the number of hours of the escalation
      *          frequency (HEF); herewith active hours/day are taken
      *          into account:
      *
      *          HRS := PREPTM - PRSPTM
      *          HEF := HRS * DYS,IZ + HRS,IZ
      *          ------------------------------------------
     C                   Z-ADD     PREPTM        D#EPTM
     C                   Z-ADD     PRSPTM        D#SPTM
     C                   EVAL      X1 = IZ
     C                   EVAL      D#WRK# = D#SPMN * 5
     C     D#WRK#        DIV       3             D#SPMN
     C                   EVAL      D#WRK# = D#EPMN * 5
     C     D#WRK#        DIV       3             D#EPMN
     C     D#EPTM        SUB       D#SPTM        D#WRK#
     C     D#WRK#        DIV       10000         D#HRS#
     C     D#HRS#        MULT      DYS(X1)       D#HEF#
     C                   ADD       HRS(X1)       D#HEF#
      *
      *          ------------------------------------------
      *          Fill parameters time from/to
      *          ------------------------------------------
     C                   EVAL      P#TIMT = %DEC(%TIME)
     C                   EVAL      P#TIMF = 0
     C                   IF        NCIETM <> 0                                  B01
     C                   Z-ADD     NCIETM        P#TIMF
     C                   ELSE                                                   X01
     C                   IF        NCINTM <> 0                                  B02
     C                   Z-ADD     NCINTM        P#TIMF
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Fill parameters date from/to
      *          ------------------------------------------
     C                   EVAL      D#DATE = 0
     C                   IF        NCIEDT <> 0                                  B01
     C                   Z-ADD     NCIEDT        D#DATE
     C                   ELSE                                                   X01
     C                   IF        NCSRDT <> 0                                  B02
     C                   Z-ADD     NCSRDT        D#DATE
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
     C                   EVAL      P#DATF = D#DATA
     C                   IF        *YEAR >= 2000                                B01
     C                   EVAL      D#CC = 1
     C                   ELSE                                                   X01
     C                   EVAL      D#CC = 0
     C                   ENDIF                                                  E01
     C                   Z-ADD     *YEAR         D#YY
     C                   EVAL      D#MM = *MONTH
     C                   EVAL      D#DD = *DAY
     C                   EVAL      P#DATT = D#DATA
      *
      *          ------------------------------------------
      *          Fill the remaining parameters for OMX931
      *          ------------------------------------------
     C                   Z-ADD     PREPTM        P#EPTM
     C                   Z-ADD     PRSPTM        P#SPTM
     C                   MOVEL     PRDOWA        P#DOWA
      *
      *          ------------------------------------------
      *          Call OMX931 and receive the difference between
      *          last escalation and now in hours (HDF)
      *          ------------------------------------------
     C                   CALLP     OMX931 ( P#DATF : P#TIMF : P#DATT :
     C                             P#TIMT : P#SPTM : P#EPTM : P#DOWA :
     C                             P#HDF# )
     C                   EVAL      D#HDF# = P#HDF#
      *
      *          ------------------------------------------
      *          Check if escalation is needed, and if so, escalate
      *          and set on escalation flag
      *
      *          If HDF >= HEF
      *             If W#WK5# <> 999
      *                W#WK5# =: W#WK5# + PIN,IZ }
      *                NCESC# =: NCESC# + 1      }   (escalate)
      *                W#SECL =: *ON
      *             End
      *          End
      *          ------------------------------------------
     C                   IF        D#HDF# >= D#HEF#                             B01
     C                             AND W#WK5# <> 999
      *
      *          ------------------------------------------
      *          escalate
      *          ------------------------------------------
     C                   EVAL      W#WKH# = W#WK5#
     C                   ADD       PIN(X1)       W#WK5#
     C                   ADD       1             NCESC#
     C                   EVAL      W#ESCL = *ON
     C                   IF        W#WK5# > 999
     C                   EVAL      W#WK5# = 999
     C                   ENDIF
      *
      *          ------------------------------------------
      *          (maybe entry program to execute)
      *          ------------------------------------------
     C                   IF        NCESC# = 0                                   B02
     C                   EXSR      SRPGMB
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Maybe "always" program to execute
      *          ------------------------------------------
     C                   EXSR      SRPGMA
      *
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          If escalation has taken place:
      *          check if escalation has to be continued, and
      *          if so, escalate as long as possible and correct
      *          after the iteration the current day only for
      *          possible remaining hours
      *
      *          If W#SECL = *ON
      *             HDF := HDF - HEF
      *             Set to first matching array element (IX)
      *             Do till HDF < HEF
      *                If priority matches, and if not maximum reached
      *                   Escalate
      *                   HDF := HDF - HEF
      *                Else
      *                   Search next matching element
      *                   If found
      *                      HEF := HRS * DYS,IY + HRS,IY
      *                      If HDF >= HEF, and if not maximum reached
      *                         Escalate
      *                         HDF := HDF - HEF
      *                      End
      *                   Else
      *                      Leave loop
      *                   End
      *                End
      *             End
      *             If remaining escalation hours
      *                Correct last escalation time for today
      *             End
      *          End
      *          ------------------------------------------
     C                   IF        W#ESCL = *ON                                 B01
     C                   SUB       D#HEF#        D#HDF#
     C                   EVAL      W#SKPI = *OFF
     C                   DOW       D#HDF# >= D#HEF#                             B02
     C                   IF        W#WK5# >= PRL(X1)                            B03
     C                             AND W#WK5# <= PRH(X1)
     C                             AND W#WK5# <> 999
      *
      *          ------------------------------------------
      *          escalate
      *          ------------------------------------------
     C                   EVAL      W#WKH# = W#WK5#
     C                   ADD       PIN(X1)       W#WK5#
     C                   ADD       1             NCESC#
     C                   SUB       D#HEF#        D#HDF#
     C                   IF        W#WK5# > 999
     C                   EVAL      W#WK5# = 999
     C                   ENDIF
      *
      *          ------------------------------------------
      *          We stay, so:
      *          maybe "always" option to be performed
      *          ------------------------------------------
     C                   EXSR      SRPGMA
      *
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          We leave, so:
      *          exit program required for execution?
      *          ------------------------------------------
     C                   IF        W#SKPI = *OFF                                B04
     C                   EXSR      SRPGME
     C                   ENDIF                                                  E04
      *
      *          ------------------------------------------
      *          Examine next one
      *          ------------------------------------------
     C                   ADD       1             X1
      *
      *          ------------------------------------------
      *          Valid if key is the same, and escalation possible
      *          ------------------------------------------
     C                   IF        HDC(X1) = NCHEDC                             B04
     C                             AND SUL(X1) = NCSUL#
     C                             AND STS(X1) = NCSTAT
     C                             AND W#WK5# < 999
      *
      *          ------------------------------------------
      *          Do we need to jump?
      *          ------------------------------------------
     C                   IF        W#WK5# > PRH(X1)                             B05
     C                   EVAL      W#SKPI = *ON
     C                   ITER
     C                   ELSE                                                   X05
     C                   EVAL      W#SKPI = *OFF
     C                   ENDIF                                                  E05
      *
     C     D#HRS#        MULT      DYS(X1)       D#HEF#
     C                   ADD       HRS(X1)       D#HEF#
     C                   IF        D#HDF# >= D#HEF#                             B05
     C                             AND W#WK5# <> 999
      *
      *          ------------------------------------------
      *          We enter, so:
      *          any program required for execution?
      *          ------------------------------------------
     C                   EXSR      SRPGMB
     C                   EXSR      SRPGMA
      *
      *          ------------------------------------------
      *          escalate
      *          ------------------------------------------
     C                   EVAL      W#WKH# = W#WK5#
     C                   ADD       PIN(X1)       W#WK5#
     C                   ADD       1             NCESC#
     C                   SUB       D#HEF#        D#HDF#
     C                   IF        W#WK5# > 999
     C                   EVAL      W#WK5# = 999
     C                   ENDIF
      *
      *          ------------------------------------------
      *          Any program required for execution?
      *          ------------------------------------------
     C                   EXSR      SRPGMA
      *
     C                   ENDIF                                                  E05
     C                   ELSE                                                   X04
     C                   LEAVE
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDDO                                                  E02
      *
      *          ------------------------------------------
      *          After the do group the escalation time computing;
      *          D#SPTM is used to transfer this value to the API
      *          ------------------------------------------
     C                   IF        D#HDF# <> 0                                  B02
     C                   EVAL      D#TIME = %DEC(%TIME)
     C                   EVAL      D#WRK# = D#MINU * 5                          Current time
     C     D#WRK#        DIV       3             D#MINU                         in decimals
     C     D#HDF#        MULT      10000         D#WRK#
     C     D#TIME        SUB       D#WRK#        D#SPTM                         Substract
     C                   EVAL      D#WRK# = D#SPMN * 3                          remainder
     C     D#WRK#        DIV       5             D#SPMN                         and convert
     C                   IF        D#SPTM <= 0                                  B03
     C                   EVAL      D#SPTM = 1
     C                   ENDIF                                                  E03
     C                   ELSE                                                   X02
     C                   EVAL      D#SPTM = 888888                              Current time
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDOWN - Check whether job should end                        * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDOWN        BEGSR
      *
      *          ------------------------------------------
      *          Process
      *          ------------------------------------------
     C                   EVAL      *IN98 = *OFF
     C                   EVAL      *IN98 = %SHTDN
     C                   IF        *IN98 = *ON                                  B01
     C     P#HEDC        CHAIN     OMEPRR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      PRJOB# = 0
     C                   MOVE      *BLANKS       PRJOBN
     C                   MOVE      *BLANKS       PRUSID
     C                   UPDATE    OMEPRR
     C                   ENDIF                                                  E02
     C                   EXSR      SRTTLR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPGMB - Check whether entry program is to be executed       * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPGMB        BEGSR
      *
      *          ------------------------------------------
      *          Control: if program found,
      *          ------------------------------------------
     C                   IF        X1 <> 0                                      B01
     C                             AND PRG(X1) <> *BLANKS
     C                             AND EOE(X1) <> '1'
     C                             AND EOE(X1) <> '3'
      *
      *          ------------------------------------------
      *          Call the programs if possible
      *          ------------------------------------------
     C                   EXSR      SRRUN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPGME - Check whether exit programs is to be executed       * /
      *          NB: defining exit programs for a range that end     *
      *              with 999 is useless                             *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPGME        BEGSR
      *
      *          ------------------------------------------
      *          Control: if program found,
      *          ------------------------------------------
     C                   IF        X1 <> 0                                      B01
     C                             AND PRG(X1) <> *BLANKS
     C                             AND EOE(X1) <> '0'
     C                             AND EOE(X1) <> '3'
      *
      *          ------------------------------------------
      *          Call the programs if possible
      *          ------------------------------------------
     C                   EXSR      SRRUN
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPGMA - Perform the "always" option                         * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPGMA        BEGSR
      *
      *          ------------------------------------------
      *          Control: if program found,
      *          ------------------------------------------
     C                   IF        X1 <> 0                                      B01
     C                             AND PRG(X1) <> *BLANKS
     C                   IF        EOE(X1) = '3'                                B02
      *
      *          ------------------------------------------
      *          if program exists:
      *          ------------------------------------------
     C                   EXSR      SRRUN
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRUN - Call the program, if possible                        * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRUN         BEGSR
      *
      *          ------------------------------------------
      *          (For security reasons: check library)
      *          ------------------------------------------
     C                   IF        LIB(X1) = *BLANKS                            B01
     C                   EVAL      LIB(X1) = C#LIBL
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          if program exists:
      *          ------------------------------------------
     C                   EVAL      P2PGMN = PRG(X1)
     C                   EVAL      P2LIBC = LIB(X1)
     C                   EVAL      P2OBJT = C#PGM
     C                   CALLP     OMH906C ( P2PGMN : P2LIBC : P2OBJT :
     C                             P2OBJD : P2STAT )
     C                   IF        P2STAT = C#NORM                              B01
      *
      *          ------------------------------------------
      *          fill parameters
      *          ------------------------------------------
     C                   MOVEL     NCHEDC        P1HEDC                         Helpdesk
     C                   MOVEL     NCINC#        P1INC#                         Incident
     C                   IF        W#WK5# >= 1000                               B02
     C                   EVAL      W#WK5# = 999
     C                   ENDIF                                                  X02
     C                   Z-ADD     W#WK5#        P1PRIN                         New priority
     C                   Z-ADD     W#WKH#        P1PRIO                         Old priority
     C                   EVAL      P1STAT = C#NORM                              Status
      *
      *          ------------------------------------------
      *          call requested program
      *          ------------------------------------------
     C                   EVAL      D#PGMD = %TRIMR(LIB(X1)) + '/'
     C                   EVAL      D#PGMD = %TRIMR(D#PGMD) + PRG(X1)
     C                   CALLP(E)  D#PGMD_001 ( P1HEDC : P1INC# : P1PRIO :
     C                             P1PRIN : P1STAT )
     C                   EVAL      *IN97 = %ERROR
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRPRNT - Print a line on the report                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRPRNT        BEGSR
      *
      *          ------------------------------------------
      *          Overflow?
      *          ------------------------------------------
     C                   IF        *IN01 = *ON                                  B01
     C                   WRITE     HEADER
     C                   EVAL      *IN01 = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Write the line
      *          ------------------------------------------
     C                   WRITE     DETAIL
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Initialization program                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Parameter definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Flag definition
      *          ------------------------------------------
     C                   EVAL      W#WK5# = *ZEROS
     C                   EVAL      W#WKH# = *ZEROS
      *
      *          ------------------------------------------
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          OMESCL1 Escalation table
      *          ------------------------------------------
     C     $SC1K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#SUL#
     C                   KFLD                    K#STAT
      *
     C     $SC1K2        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#SUL#
     C                   KFLD                    K#STAT
     C                   KFLD                    K#PRL#
      *
     C     $SC1K3        KLIST
     C                   KFLD                    K#HEDC
      *
      *          ------------------------------------------
      *          Keylists OMINCL3
      *          ------------------------------------------
     C     $NC3K1        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#SUL#
     C                   KFLD                    K#STAT
      *
     C     $NC3K2        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#SUL#
     C                   KFLD                    K#STAT
     C                   KFLD                    K#PRI#
      *
      *          ------------------------------------------
      *          Parameter list
      *          ------------------------------------------
     C     $MDEXC        PLIST
     C                   PARM                    P#CMD
     C                   PARM      20            P#CMD#
      *
      *          ------------------------------------------
      *          Parameter list for OMX931
      *          ------------------------------------------
     C     $MX931        PLIST
     C                   PARM                    P#DATF
     C                   PARM                    P#TIMF
     C                   PARM                    P#DATT
     C                   PARM                    P#TIMT
     C                   PARM                    P#SPTM
     C                   PARM                    P#EPTM
     C                   PARM                    P#DOWA
     C                   PARM                    P#HDF#
      *
      *          ------------------------------------------
      *          Parameter list for OMQCHGIN
      *          ------------------------------------------
     C     $CHGIN        PLIST
     C                   PARM      C#INTL        I4INTL
     C                   PARM      '33'          I4ACTC
     C                   PARM                    I4BUF
     C                   PARM                    I4PRC
     C                   PARM                    I4ERR
     C                   PARM                    I4FLD
      *
      *          ------------------------------------------
      *          Parameter list for OMH904C (Get message data text)
      *          ------------------------------------------
     C     $X904C        PLIST
     C                   PARM                    MSGID
     C                   PARM      'OMHDSP'      MSGF
     C                   PARM      '*LIBL'       MSGLIB
     C                   PARM                    MSGTXT
      *
      *          ------------------------------------------
      *          Parameter list for entry/exit program
      *          ------------------------------------------
     C     $OEPGM        PLIST
     C                   PARM                    P1HEDC
     C                   PARM                    P1INC#
     C                   PARM                    P1PRIO
     C                   PARM                    P1PRIN
     C                   PARM                    P1STAT
      *
      *          ------------------------------------------
      *          Parameter list for OMH906C (Check object)
      *          ------------------------------------------
     C     $X906C        PLIST
     C                   PARM                    P2PGMN
     C                   PARM                    P2LIBC
     C                   PARM      C#PGM         P2OBJT
     C                   PARM                    P2OBJD
     C                   PARM                    P2STAT
      *
      *          ------------------------------------------
      *          Set status
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#NORM
      *
      *          ------------------------------------------
      *          Continue?
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMEPRR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   EVAL      W#VALI = *ON
     C                   Z-ADD     D#JOB#        PRJOB#
     C                   MOVEL     D#JOBN        PRJOBN
     C                   MOVEL     D#USID        PRUSID
     C                   UPDATE    OMEPRR
     C                   ELSE                                                   X01
     C                   EVAL      W#VALI = *OFF
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Store escalation table, start with positioning
      *          ------------------------------------------
     C                   IF        P#HEDC = '*ALL'                              B01
     C                   MOVE      *LOVAL        K#HEDC
     C                   EVAL      *IN97 = *OFF
     C                   ELSE                                                   X01
     C                   EVAL      K#HEDC = P#HEDC
     C                   EVAL      *IN97 = *ON
     C                   ENDIF                                                  E01
     C                   MOVE      *LOVAL        K#SUL#
     C                   MOVE      *LOVAL        K#STAT
     C                   MOVE      *LOVAL        K#PRL#
     C                   EVAL      W#EOFI = *OFF
     C                   CLEAR                   HDC
     C                   EVAL      IZ = 0
     C     $SC1K2        SETLL     OMESCR
      *
      *          ------------------------------------------
      *          Read all HelpDesks in case if *ALL, otherwise confine
      *          to requested helpdesk
      *          ------------------------------------------
     C                   DOW       W#EOFI <> *ON                                B01
     C                   IF        *IN97 = *OFF
     C                   READ      OMESCR
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN97 = *ON
     C     $SC1K3        READE     OMESCR
     C                   EVAL      *IN99 = %EOF
     C                   ENDIF
     C                   IF        *IN99 = *ON                                  B02
     C                             OR IZ = 9999                                 B02
     C                   EVAL      W#EOFI = *ON
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Only the first valid record is used in case
      *          of complete overlap
      *          ------------------------------------------
     C                   EVAL      IY = IZ
     C                   IF        IZ = 0                                       B03
     C                             OR SCPRH# > PRH(IY)
     C                             OR SCHEDC <> HDC(IY)
     C                             OR SCSUL# <> SUL(IY)
     C                             OR SCSTAT <> STS(IY)
     C                   ADD       1             IZ
     C                   MOVEL     SCHEDC        HDC(IZ)
     C                   MOVE      SCSUL#        SUL(IZ)
     C                   MOVEL     SCSTAT        STS(IZ)
      *
      *          ------------------------------------------
      *          Partly overlap: adjust record
      *          ------------------------------------------
     C                   IF        IY <> 0                                      B04
     C                             AND PRH(IY) >= SCPRL#
     C                             AND SCHEDC = HDC(IY)
     C                             AND SCSUL# = SUL(IY)
     C                             AND SCSTAT = STS(IY)
     C     PRH(IY)       ADD       1             PRL(IZ)
     C                   ELSE                                                   X04
     C                   Z-ADD     SCPRL#        PRL(IZ)
     C                   ENDIF                                                  E04
     C                   Z-ADD     SCPRH#        PRH(IZ)
     C                   Z-ADD     SCPIN#        PIN(IZ)
     C                   Z-ADD     SCDYS#        DYS(IZ)
     C                   Z-ADD     SCHRS#        HRS(IZ)
     C                   MOVEL     SCPGMN        PRG(IZ)
     C                   MOVEL     SCLIBC        LIB(IZ)
     C                   MOVEL     SCEOEI        EOE(IZ)
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDDO                                                  E01
      *
      *          ------------------------------------------
      *          Seton the overflow line.
      *          ------------------------------------------
     C                   EVAL      *IN01 = *ON
     C                   EXSR      SRGVAR
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
     C                   FOR       MS = 1 TO 5                                  B01
     C                   EVAL      MSGID = MSD(MS)
      *
      *          ------------------------------------------
      *          Call subprogram
      *          ------------------------------------------
     C                   IF        MSGID <> *BLANKS                             B02
     C                   EVAL      MSGF = 'OMHDSP'
     C                   EVAL      MSGLIB = '*LIBL'
     C                   CALLP     OMH904C ( MSGID : MSGF : MSGLIB : MSGTXT
     C                             )
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Fill message array
      *          ------------------------------------------
     C                   EVAL      VAR(MS) = MSGTXT
     C                   ENDFOR                                                 E01
      *
      *          ------------------------------------------
      *          Fill message gotten indicator
      *          ------------------------------------------
     C                   EVAL      MSGGET = '1'
      *
      *          ------------------------------------------
      *          Fill the printerfields.
      *          ------------------------------------------
     C                   MOVEL     VAR(1)        VAR01
     C                   MOVEL     VAR(2)        VAR02
     C                   MOVEL     VAR(3)        VAR03
     C                   MOVEL     VAR(4)        VAR04
     C                   MOVEL     VAR(5)        VAR05
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
      *          Indicate monitor no longer active
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMEPRR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   EVAL      PRJOB# = 0
     C                   MOVE      *BLANKS       PRJOBN
     C                   MOVE      *BLANKS       PRUSID
     C                   UPDATE    OMEPRR
     C                   ENDIF                                                  E02
      *
      *          ------------------------------------------
      *          Post information about the last used file
      *          ------------------------------------------
     C                   SELECT                                                 B02
     C                   WHEN      E#FILE = 'OMH026  '                          X02
     C                   MOVEL     MH026         P#FILE
      *
     C                   WHEN      E#FILE = 'OMINCL1 '                          X02
     C                   MOVEL     INC           P#FILE
      *
     C                   WHEN      E#FILE = 'OMEPRL1 '                          X02
     C                   MOVEL     EPR           P#FILE
      *
     C                   WHEN      E#FILE = 'OMESCL1 '                          X02
     C                   MOVEL     ESC           P#FILE
      *
     C                   ENDSL                                                  E02
      *
      *          ------------------------------------------
      *          Set status
      *          ------------------------------------------
     C                   EVAL      P#STAT = C#TERM
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
      *          Seton last record, close program
      *          ------------------------------------------
     C                   CALLP     OMQCHGIN_001()
     C                   IF        *IN01 = *OFF
     C                   WRITE     FOOTER
     C                   ENDIF
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
**CTDATA MSD
MH02601  - Header 1
HED0001  - Helpdesk code
MH02602  - List header 1
MH02603  - List header 2
MH02604  - End of report
**CTDATA CMD
DLYJOB DLY(xxxxxxxx)
