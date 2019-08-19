     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Program ....: OMH045                                        *
      * Function ...: Retrieve mail template for contact            *
      * Author .....: Remain Software                               *
      *                                                             *
      * Input ......: P#HEDC  Helpdesk                              *
      *               P#CUSC  Customer                              *
      *               P#CCTC  Contact                               *
      *               P#ACTC  Actioncode                            *
      *                                                             *
      * Output .....: P#MATL  Library                               *
      *               P#MATF  Sourcefile containing template        *
      *               P#MATM  Template member name                  *
      *                                                             *
      *               P#STAT  *NORM  *NOTF  *TERM                   *
      *                                                             *
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
     FOMTEML1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(TEML1)
     FOMMHXL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(MHXL1)
     FOMMPXL1   IF   E           K DISK    INFSR(*PSSR)
     F                                     INFDS(MPXL1)
     FOMMSX     IF   E             DISK    INFSR(*PSSR)
     F                                     INFDS(MSX)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Array definitions                                           *
      * ----------------------------------------------------------- *
     D COD             S             10    DIM(4)
      *COPY QCPYSRC,OMSREFREN
      *
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
     D TEML1           DS            57
     D MHXL1           DS            57
     D MPXL1           DS            57
     D MSX             DS            57
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
      *          Named constants for status field
      *          ------------------------------------------
     D C#NONE          C                   CONST('*NONE')
     D C#NOTF          C                   CONST('*NOTF')
     D C#TERM          C                   CONST('*TERM')
     D C#NORM          C                   CONST('*NORM')
     D C#LIBL          C                   CONST('*LIBL')
      * Prototype for OMH045
     D OMH045          PR
     D P#HEDC_                             LIKE(P#HEDC)
     D P#CUSC_                             LIKE(P#CUSC)
     D P#CCTC_                             LIKE(P#CCTC)
     D P#ACTC_                             LIKE(P#ACTC)
     D P#MATL_                             LIKE(P#MATL)
     D P#MATF_                             LIKE(P#MATF)
     D P#MATM_                             LIKE(P#MATM)
     D P#STAT_                             LIKE(P#STAT)
      * Prototype for 'OMX942C'
     D OMX942C         PR                  EXTPGM('OMX942C')
     D P#OBJC_                             LIKE(P#OBJC)
     D P#OBJL_                             LIKE(P#OBJL)
     D P#MBRC_                             LIKE(P#MBRC)
     D P#MBRA_                             LIKE(P#MBRA)
     D P#MBRD_                             LIKE(P#MBRD)
     D P#STAT_                             LIKE(P#STAT)
      *
      *          ------------------------------------------
      *          OMTEM Template file
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
     D OMH045          PI
     D P#HEDC                         5
     D P#CUSC                        10
     D P#CCTC                        10
     D P#ACTC                         2
     D P#MATL                        10
     D P#MATF                        10
     D P#MATM                        10
     D P#STAT                         5
      *----------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *----------------------------------------------------------------------
     D I#PSSR          S              1
     D K#ACTC          S                   LIKE(ACTC)
     D K#CCTC          S                   LIKE(CCTC)
     D K#CUSC          S                   LIKE(CUSC)
     D K#HEDC          S                   LIKE(HEDC)
     D P#FILE          S             57
     D P#MBRA          S             10
     D P#MBRC          S             10
     D P#MBRD          S             50
     D P#MSKC          S              4
     D P#MSTA          S             40
     D P#OBJC          S             10
     D P#OBJL          S             10
     D P#PMQC          S             10
     D W#VALI          S                   LIKE(INDI)
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
     C                   SELECT
     C                   WHEN      P#STAT = C#NORM                              B01
     C                   EXSR      SRVERW                                       B01
     C                   ENDSL
      *
      *          ------------------------------------------
      *          Return
      *          ------------------------------------------
     C                   EXSR      SRTTLR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVERW - Processing                                          * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVERW        BEGSR
      *
      *          ------------------------------------------
      *          Check default indication
      *          ------------------------------------------
     C                   EXSR      SRDFTI
      *
      *          ------------------------------------------
      *          Mail is not used at all
      *          ------------------------------------------
     C                   IF        PXMTDI <> '9'
      *
      *          ------------------------------------------
      *          At contact level: look up template
      *          ------------------------------------------
     C                   EVAL      K#HEDC = *BLANKS
     C                   EVAL      K#CUSC = P#CUSC
     C                   EVAL      K#CCTC = P#CCTC
     C                   EVAL      K#ACTC = P#ACTC
     C     $EM1K4        CHAIN     OMTEMR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                   MOVEL     EMMATM        P#MATM
     C                   EXSR      SRVAL
     C                   ENDIF                                                  E01
     C                   IF        W#VALI = *ON                                 B01
     C                   EXSR      SRFILL
     C                   ELSE                                                   X01
      *
      *          ------------------------------------------
      *          Not found, not valid: try at customer level
      *                                if default-indicator allows us
      *          ------------------------------------------
     C                   IF        PXMTDI = '1'
     C                             OR PXMTDI = '2'
     C                   EVAL      K#CCTC = *BLANKS
     C                   EVAL      K#HEDC = *BLANKS
     C     $EM1K4        CHAIN     OMTEMR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B02
     C                   MOVEL     EMMATM        P#MATM
     C                   EXSR      SRVAL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
     C                   IF        W#VALI = *ON                                 B02
     C                   EXSR      SRFILL
     C                   ELSE                                                   X02
      *
      *          ------------------------------------------
      *          Not found, not valid: try at helpdesk level
      *                                if default-indicator allows us
      *          ------------------------------------------
     C                   IF        PXMTDI = '1'
     C                             OR PXMTDI = '3'
     C                   EVAL      K#HEDC = P#HEDC
     C                   EVAL      K#CUSC = *BLANKS
     C                   EVAL      K#CCTC = *BLANKS
     C     $EM1K4        CHAIN     OMTEMR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B03
     C                   MOVEL     EMMATM        P#MATM
     C                   EXSR      SRVAL
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E03
     C                   IF        W#VALI = *ON                                 B03
     C                   EXSR      SRFILL
     C                   ELSE                                                   X03
      *
      *          ------------------------------------------
      *          Not found, not valid: try at system level if necessary
      *          ------------------------------------------
     C                   IF        PXMTDI = '1'
     C                             OR PXMTDI = '4'
     C                   EVAL      K#HEDC = *BLANKS
     C                   EVAL      K#CUSC = *BLANKS
     C                   EVAL      K#CCTC = *BLANKS
     C     $EM1K4        CHAIN     OMTEMR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B04
     C                   MOVEL     EMMATM        P#MATM
     C                   EXSR      SRVAL
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E04
     C                   IF        W#VALI = *ON                                 B04
     C                   EXSR      SRFILL
     C                   ELSE                                                   X04
     C                   ENDIF                                                  E04
     C                   ENDIF                                                  E03
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          "Endifs" for the default indicator
      *          ------------------------------------------
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          All searches in vain: "not found"
      *          ------------------------------------------
     C                   IF        W#VALI = *OFF                                B01
     C                   EVAL      P#STAT = C#NOTF
     C                   EVAL      %SUBST(P#MATM:1:5) = C#NONE
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRDFTI - Get default indicator                               * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRDFTI        BEGSR
      *
      *          ------------------------------------------
      *          Chain mail file
      *          ------------------------------------------
     C                   EVAL      K#CUSC = P#CUSC
     C                   EVAL      K#CCTC = P#CCTC
     C     $PX1K2        CHAIN     OMMPXR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *ON                                  B01
     C                   MOVE      '9'           PXMTDI
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ----------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------ *
      * SRVAL  - Check whether source member exists                  * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRVAL         BEGSR
      *
      *          ------------------------------------------
      *          At helpdesk level
      *          ------------------------------------------
     C                   IF        COD(1) <> *BLANKS                            B01
     C                   EVAL      P#OBJC = COD(1)
     C                   EVAL      P#OBJL = COD(2)
     C                   MOVEL     EMMATM        P#MBRC
     C                   CALLP     OMX942C ( P#OBJC : P#OBJL : P#MBRC :
     C                             P#MBRA : P#MBRD : P#STAT )
     C                   IF        P#STAT = C#NORM                              B02
     C                             AND P#MBRA <> *BLANKS
     C                   EVAL      W#VALI = *ON
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          At system level
      *          ------------------------------------------
     C                   IF        COD(3) <> *BLANKS                            B01
     C                             AND W#VALI <> *ON
     C                   EVAL      P#OBJC = COD(3)
     C                   EVAL      P#OBJL = COD(4)
     C                   MOVEL     EMMATM        P#MBRC
     C                   CALLP     OMX942C ( P#OBJC : P#OBJL : P#MBRC :
     C                             P#MBRA : P#MBRD : P#STAT )
     C                   IF        P#STAT = C#NORM                              B02
     C                             AND P#MBRA <> *BLANKS
     C                   EVAL      W#VALI = *ON
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
      * SRFILL - Fill the parameters                                 * /
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRFILL        BEGSR
      *
      *          ------------------------------------------
      *          Fill
      *          ------------------------------------------
     C                   MOVEL     EMMATM        P#MATM
     C                   EVAL      P#MATF = P#OBJC
     C                   EVAL      P#MATL = P#OBJL
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
      *          Keyfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Workfield definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Parameter list for OMX942C (Retrieve member descr)
      *          ------------------------------------------
     C     $X942C        PLIST
     C                   PARM                    P#OBJC
     C                   PARM                    P#OBJL
     C                   PARM                    P#MBRC
     C                   PARM                    P#MBRA
     C                   PARM                    P#MBRD
     C                   PARM                    P#STAT
      *
      *          ------------------------------------------
      *          OMTEM Template file
      *          ------------------------------------------
     C     $EM1K4        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
     C                   KFLD                    K#ACTC
     C     $EM1K3        KLIST
     C                   KFLD                    K#HEDC
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
      *
      *          ------------------------------------------
      *          OMMPX Contact mail file
      *          ------------------------------------------
     C     $PX1K2        KLIST
     C                   KFLD                    K#CUSC
     C                   KFLD                    K#CCTC
      *
      *          ------------------------------------------
      *          Set
      *          ------------------------------------------
     C                   EVAL      W#VALI = *OFF
     C                   EVAL      P#STAT = C#NORM
     C                   CLEAR                   COD
      *
      *          ------------------------------------------
      *          Get sourcefile and library from helpdesk
      *          ------------------------------------------
     C     P#HEDC        CHAIN     OMMHXR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                             AND HXMATF <> *BLANKS
     C                   MOVEL     HXMATF        COD(1)
     C                   IF        HXMATL <> *BLANKS                            B02
     C                   MOVEL     HXMATL        COD(2)
     C                   ELSE                                                   X02
     C                   EVAL      COD(2) = C#LIBL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          Get sourcefile and library from system
      *          ------------------------------------------
     C     1             CHAIN     OMMSXR
     C                   EVAL      *IN99 = NOT %FOUND
     C                   IF        *IN99 = *OFF                                 B01
     C                             AND SXMATF <> *BLANKS
     C                   MOVEL     SXMATF        COD(3)
     C                   IF        SXMATL <> *BLANKS                            B02
     C                   MOVEL     SXMATL        COD(4)
     C                   ELSE                                                   X02
     C                   EVAL      COD(4) = C#LIBL
     C                   ENDIF                                                  E02
     C                   ENDIF                                                  E01
      *
      *          ------------------------------------------
      *          No location to search: "not found"
      *          ------------------------------------------
     C                   IF        COD(1) = *BLANKS                             B01
     C                             AND COD(3) = *BLANKS
     C                   EVAL      P#STAT = C#NOTF
     C                   EVAL      %SUBST(P#MATM:1:5) = C#NONE
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
      *
     C                   WHEN      E#FILE = 'OMTEML1 '                          X02
     C                   MOVEL     TEML1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMHXL1 '                          X02
     C                   MOVEL     MHXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMPXL1 '                          X02
     C                   MOVEL     MPXL1         P#FILE
      *
     C                   WHEN      E#FILE = 'OMMSX   '                          X02
     C                   MOVEL     MSX           P#FILE
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
      *          Seton last record
      *          ------------------------------------------
     C                   EVAL      *INLR = *ON
     C                   RETURN
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
