      * ------------------------------------------------------------- *
      * Description                                                   *
      * ------------------------------------------------------------- *
      *                                                               *
      * Copyright ..: Remain BV, Netherlands. All rights reserved     *
      *                                                               *
      * Program ....: OMH037                                          *
      * Function ...: Receive distributions                           *
      * Author .....: Remain Software                                 *
      * Parameters .: P#Opti - '1' - Process from distribution        *
      *                        '2' - Process from OMMTO               *
      *               P#STAT - Helpdesk                               *
      *                                                               *
      * This program receives the distributions that arrived for      *
      * the helpdesk.                                                 *
      *                                                               *
      * ------------------------------------------------------------- *

     FOmHmt     O  A E             DISK    INFDS(HmtL1)
     FOmHmq     O  A E             DISK    INFDS(HmqL1)

     FOmHmhL1   UF A E           K DISK    INFDS(HmhL1)
     FOmHmoL1   UF A E           K DISK    INFDS(HmoL1)

     FOmIncL1   IF   E           K DISK    INFDS(IncL1)
     FOmCcaL1   IF   E           K DISK    INFDS(CcaL1)
     FOmQlqL1   IF   E           K DISK    INFDS(QlqL1)
     FOmQlaL1   IF   E           K DISK    INFDS(QlaL1)
     FOmMhxL1   IF   E           K DISK    INFDS(MhxL1)

     FOmErf     IF   E             DISK    INFDS(ErfL1)
     FOmDiq     IF   E             DISK    INFDS(DiqL1)

      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Input specifications                                          *
      * ------------------------------------------------------------- *

      *          ------------------------------------------
      *          Field reference file used to define fields
      *          ------------------------------------------
     D FldRef        E DS                  ExtName(OmsRefhd)

      *          ------------------------------------------
      *          File information data structure
      *          ------------------------------------------
     D HmhL1           DS            57
     D HmtL1           DS            57
     D HmqL1           DS            57
     D HmoL1           DS            57
     D ErfL1           DS            57
     D IncL1           DS            57
     D CcaL1           DS            57
     D DiqL1           DS            57
     D QlqL1           DS            57
     D QlaL1           DS            57
     D MhxL1           DS            57

      *          ------------------------------------------
      *          Program data structure
      *          ------------------------------------------
     D PGM            SDS
     D  D#PARM           *PARMS

      *          ------------------------------------------
      *          Array holding the Q&A
      *          ------------------------------------------
     DQnA              DS
     D  QnAA                          8
     D   D#Qls#                       5S 0 Overlay(QnAA)
     D   D#Qsq#                       3S 0 Overlay(QnAA:6)

      *          ------------------------------------------
      *          Answer
      *          ------------------------------------------
     D                 DS
     D  D#Ans#                        3S 0
     D   D#Ansc                       3    Overlay(D#Ans#)

      *          ------------------------------------------
      *          Route entry seqeuce
      *          ------------------------------------------
     D                 DS
     D  D#Res#                        3S 0
     D   D#Resn                       3    Overlay(D#Res#)

      *          ------------------------------------------
      *          Incident description Array
      *          ------------------------------------------
     D  D#Incd         S              1A   DIM(70)


      *          ------------------------------------------
      *          Pointer to the OMH037 user space
      *          ------------------------------------------
     D  PtrToUsp       S               *   Inz(*Null)
     DOmH037           DS         32767    Based(PtrToUsp)
     D   A#H037                       1    Dim(32767)

      *          ------------------------------------------
      *          Error buffer for API call
      *          ------------------------------------------
      /COPY QAPILESRC,ERR_BUFFER

      *          ------------------------------------------
      *          Binary number definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFBINARY

      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#Term          C                   CONST('*TERM')
     D C#Norm          C                   CONST('*NORM')
     D C#New           C                   CONST('*NEW ')
     D C#Uspq          C                   CONST('OMH037    QTEMP    ')
     D C#Cbo           C                   CONST('OMS - Created by OMS')
     D C#All           C                   CONST('*ALL')
     D C#CrLf          C                   CONST(X'0D25')

      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Main line                                                   *
      * ------------------------------------------------------------- *

      *          ------------------------------------------
      *          Entry parameters
      *          ------------------------------------------
     C     *Entry        Plist
     C                   Parm                    P#Opti
     C                   Parm                    P#Stat

      *          ------------------------------------------
      *          Close if no parameters passed.
      *          ------------------------------------------
     C     D#Parm        Ifeq      *Zeros                                       B01
     C                   Exsr      SrTtlr
     C                   Endif                                                  E01
      *
      *          ------------------------------------------
      *          Do the processing routine
      *          ------------------------------------------
     C                   Exsr      SrVerw
      *
      *          ------------------------------------------
      *          Close the create incident program
      *          ------------------------------------------
     C                   Call      'OMH037_3'
      *
      *          ------------------------------------------
      *          Return to caller.
      *          ------------------------------------------
     C                   Seton                                        LR
     C                   Return
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SrVerw - Processing                                           *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SrVerw        Begsr

      *    ----------------------------------------------------
      *    Either process from new or original distribution
      *    ----------------------------------------------------
     C     P#Opti        Caseq     '1'           SrNew
      **** P#Opti        Caseq     '2'           SrOld
     C                   Endcs
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   Endsr
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SrNew  - Process new mail                                     *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SrNew         Begsr

      *    ----------------------------------------------------
      *    Read the mail distribution records and process them.
      *    ----------------------------------------------------
     C                   Read      OmDiq                                  99
     C     *In99         Doweq     *OFF

      *    ----------------------------------------------------
      *    Offset to the user space = 0
      *    ----------------------------------------------------
     C                   Z-add     1             P#Stp#

      *    ----------------------------------------------------
      *    Fill the header file.
      *    ----------------------------------------------------
     C                   Move      LinDid        MhDsid
     C                   Move      LinSui        MhUsri
     C                   Move      LinSua        MhAddr
     C                   Movel     LinTxd        MhSubj
     C                   Move      *Blanks       MhHedc
     C                   Move      *Blanks       MhInc#
     C                   Move      *Blanks       MhCusc
     C                   Move      *Blanks       MhCctc
     C                   Move      *Blanks       MhRuid
     C                   Move      *Blanks       MhCsrc
     C                   Move      C#New         MhStat

      *    ----------------------------------------------------
      *    Read the first record from the data file
      *    ----------------------------------------------------
     C                   Read      OmErf                                  99

      *    ----------------------------------------------------
      *    Document size, process not more than 3000 bytes.
      *    ----------------------------------------------------
     C                   Z-Add     RcvDsz        W#Dsz
     C     W#Dsz         IfGt      3000
     C                   Z-Add     3000          W#Dsz
     C                   EndIf

      *    ----------------------------------------------------
      *    If still matching records.
      *    ----------------------------------------------------
     C     *In99         Doweq     *Off                                         B01
     C     LinDid        Andeq     RcvDid

      *    ----------------------------------------------------
      *    Fill the user space
      *    ----------------------------------------------------
     C                   Eval      %Subst(OmH037: P#Stp#: 500) = RcvDta

     C     P#Stp#        Iflt      3000
     C                   Add       500           P#Stp#
     C                   Endif

      *    ----------------------------------------------------
      *    Fill the original distribution file
      *    ----------------------------------------------------
     C                   Move      RcvDid        MoDsid
     C                   Z-add     RcvSno        MoSeq#
     C                   Move      RcvDta        MoDdta
     C                   Write     OmHmoR

      *    ----------------------------------------------------
      *    Read the next OmErf
      *    ----------------------------------------------------
     C                   Read      OmErf                                  99
     C                   Enddo

      *    ----------------------------------------------------
      *    Fill the files, write the header.
      *    ----------------------------------------------------
     C                   Exsr      SrScan
     C                   Write     OmHmhR
     C                   Move      *Blanks       OmH037

      *          ----------------------------------------
      *          Check the distribtion id. If normal then
      *          continue and process the data.
      *          ----------------------------------------
     C     MhDsid        Chain     OmhmhL1                            99
     C                   Call      'OMH037_2'    $Mh037_2
     C                   Move      P#Stat        MhStat
     C                   Update    OmHmhR

      *          ----------------------------------------
      *          If the distribution contains errors then check
      *          if someone should be notified and notify.
      *          ----------------------------------------
     C     MhHedc        Chain     OmMhxL1                            99
     C   99              Move      *Off          HxApii

     C                   If        HxSwtu <> *Blanks and P#Stat <> C#Norm
     C                   Call      'OMH966C'     $Mx966C
     C                   Endif

      *          ----------------------------------------
      *          Check if we must create an incident automagically.
      *          ----------------------------------------
     C                   If        MhStat <> C#Term and HxApii = *On
     C                   Call      'OMH037_3'    $Mh037_3
     C                   Endif

      *    ----------------------------------------------------
      *    Read the next distribution record
      *    ----------------------------------------------------
     C                   Read      OmDiq                                  99
     C                   Enddo                                                  E01

      *    ----------------------------------------------------
      *    End of subroutine
      *    ----------------------------------------------------
     C                   Endsr

      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SrScan - Scan the details                                     *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SrScan        Begsr

      *          ----------------------------------------
      *          Get the fields.
      *          ----------------------------------------
     C                   Exsr      SrFld

      *          ----------------------------------------
      *          Get the question and answers
      *          ----------------------------------------
     C                   Exsr      SrQna

      *          ----------------------------------------
      *          Get the descriptions.
      *          ----------------------------------------
     C                   Exsr      SrDsc

      *          ----------------------------------------
      *          Fill missing data if the incident could be found.
      *          ----------------------------------------
     C     $Nc1K1        Setll     OmIncL1                                99
     C                   If        *In99
     C     $Nc1K1        Chain     OmIncL1                            99
     C     $Ca1K1        Chain     OmCcaL1                            99

     C     MhCusc        Ifeq      *Blanks
     C                   Move      CaCusc        MhCusc
     C                   Endif

     C     MhCctc        Ifeq      *Blanks
     C                   Move      CaCctc        MhCctc
     C                   Endif

     C     MhCsrc        Ifeq      *Blanks
     C                   Move      CaCsrc        MhCsrc
     C                   Endif

     C     MhRuid        Ifeq      *Blanks
     C                   Move      NcRuid        MhRuid
     C                   Endif

     C     MhShid        Ifeq      *Blanks
     C                   Move      NcShid        MhShid
     C                   Endif
     C                   Endif

      *          ----------------------------------------
      *          End of subroutine
      *          ----------------------------------------
     C                   Endsr
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SrFld  - Retrieve the incident header fields.                 *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SrFld         Begsr

      *          ------------------------------------------
      *          Reset the scan index
      *          ------------------------------------------
     C                   Z-add     *Zeros        Sc

      *          ------------------------------------------
      *          HelpDesk
      *          ------------------------------------------
     C     MhHedc        Ifeq      *BLANKS                                      B01
     C     'HEDC'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *ON                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *In99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MhHedc
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Incident
      *          ------------------------------------------
     C     MhInc#        Ifeq      *Blanks                                      B01
     C     'INCN'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *In99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MhInc#
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Short incident description
      *          ------------------------------------------
     C     MhShid        Ifeq      *Blanks                                      B01
     C     'SHID'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *In99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Ifle      50
     C     St            Subst     OmH037:Sc     MhShid
     C                   Else
     C     50            Subst     OmH037:Sc     MhShid
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Allocated by user
      *          ------------------------------------------
     C     MhRuid        Ifeq      *BLANKS                                      B01
     C     'RUID'        SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MhRuid
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Customer code
      *          We try to scan to newline (X'0D'). If this fails
      *          then we search until the first space.
      *          ------------------------------------------
     C     MhCusc        Ifeq      *BLANKS                                      B01
     C     'CUSC'        SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St

      *                  ----------------------------------
      *                  If newline found then check if this is within limit
      *                  ----------------------------------
     C     St            Ifgt      *Zeros
     C     St            Andle     10
     C     St            Subst     OmH037:Sc     MhCusc

      *                  ----------------------------------
      *                  No, scan for the first space.
      *                  ----------------------------------
     C                   else
     C                   Z-add     Sc            St
     C     ' '           SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Andle     10
     C     St            Subst     OmH037:Sc     MhCusc
     C                   Endif
     C                   Endif
     C                   Endif                                                  E03

     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Contact code
      *          We try to scan to newline (X'0D'). If this fails
      *          then we search until the first space.
      *          ------------------------------------------
     C     MhCctc        Ifeq      *BLANKS                                      B01
     C     'CCTC'        SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St

      *                  ----------------------------------
      *                  If newline found then check if this is within limit
      *                  ----------------------------------
     C     St            Ifgt      *Zeros
     C     St            Andle     10
     C     St            Subst     OmH037:Sc     MhCctc

      *                  ----------------------------------
      *                  No, scan for the first space.
      *                  ----------------------------------
     C                   else
     C                   Z-add     Sc            St
     C     ' '           SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Andle     10
     C     St            Subst     OmH037:Sc     MhCctc
     C                   Endif
     C                   Endif
     C                   Endif                                                  E03

     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Call Source Code
      *          ------------------------------------------
     C     MhCsrc        Ifeq      *BLANKS                                      B01
     C     'CSRC'        SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MhCsrc
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Status route
      *          ------------------------------------------
     C     MhStrc        Ifeq      *BLANKS                                      B01
     C     'STRC'        SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MhStrc
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Route entry sequence.
      *          ------------------------------------------
     C     MhRes#        Ifeq      *Zeros                                       B01
     C     'RESN'        SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifeq      3
     C     St            Subst     OmH037:Sc     D#Resn
     C                   Z-add     D#Res#        MhRes#
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SrQna  - Retrieve the question and answer                     *
      *                                                               *
      * The question and answer sequences arrive in the form          *
      * nnnnxxx where nnnn is the question list number and xxx is     *
      * the question number in the question list. Before we can parse *
      * the questions, it is obvious that we must know the helpdesk   *
      * code. If one is not found or it stinks then we can not        *
      * parse.                                                        *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SrQna         BEGSR

      *          ------------------------------------------
      *          Init
      *          ------------------------------------------
     C                   Move      *all'0'       QnAA
     C                   Z-add     *Zeros        IX

      *          ------------------------------------------
      *          Read the first question from this helpdesk.
      *          ------------------------------------------
     C     MhHedc        Chain     OmQlaL1                            99

      *          ------------------------------------------
      *          Fill question by question
      *          ------------------------------------------
     C                   Z-add     LaQls#        D#Qls#
     C                   Z-add     LaQsq#        D#Qsq#

      *          ------------------------------------------
      *          Scan all questions
      *          ------------------------------------------
     C                   Dow       1=1

      *          ------------------------------------------
      *          Check if this question is answered
      *          ------------------------------------------
     C     QnAA          SCAN      OmH037        Sc                       99
     C     *IN99         Ifeq      *On                                          B02
     C                   Add       9             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *IN99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C                   Move      LinDid        MqDSid
     C                   Z-add     D#Qls#        MqQls#
     C                   Z-add     D#Qsq#        MqQsq#
     C     St            Subst     OmH037:Sc     D#Ansc
     C                   Z-add     D#Ans#        MqAns#
     C                   Write     OmHmqR

      *          ------------------------------------------
      *          Answer found. What is the action for this answer?
      *          ------------------------------------------
     C     $La1K2        Chain     OmQlaL1                            99
     C   99              Leave
     C     $La1K1        Chain     OmQlaL1                            99
     C   99              Leave

      *          ------------------------------------------
      *          Get the next question  (if any)
      *          ------------------------------------------
     C                   Z-add     LaGls#        D#Qls#
     C                   Z-add     LaGqs#        D#Qsq#

     C                   If        LaQls# <> *Zeros
     C                   Iter
     C                   Endif

     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

     C                   Leave
     C                   Enddo                                                  E01

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * SrDsc  - Retrieve the description fields                      *
      *                                                               *
      * ------------------------------------------------------------- *
     C     SrDsc         Begsr

      *          ------------------------------------------
      *          Init
      *          ------------------------------------------
     C                   Z-add     *Zeros        Sc
     C                   Move      LinDid        MtDsid
     C                   Z-add     10            MtSeq#
     C                   Move      *Blanks       MtSitd
     C                   Move      *Blanks       MtIncd
     C                   Move      *Blanks       MtSul#
     C                   Move      *Blanks       MtItxt

      *          ------------------------------------------
      *          Short header
      *          ------------------------------------------
     C     MtSitd        Ifeq      *Blanks                                      B01
     C     'SITD'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *In99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Ifle      50
     C     St            Subst     OmH037:Sc     MtSitd
     C                   Else
     C     50            Subst     OmH037:Sc     MtSitd
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Support level
      *          ------------------------------------------
     C     MtSul#        Ifeq      *Blanks                                      B01
     C     'SULN'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *In99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MtSul#
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Text type
      *          ------------------------------------------
     C     MtItxt        Ifeq      *Blanks                                      B01
     C     'ITXT'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *On                                          B02
     C                   Add       5             Sc
     C                   Z-add     Sc            St
     C     X'0D'         SCAN      OmH037:St     St                       99
     C     *In99         Ifeq      *ON                                          B03
     C                   Sub       Sc            St
     C     St            Ifgt      *Zeros
     C     St            Subst     OmH037:Sc     MtSul#
     C                   Endif                                                  E03
     C                   Endif                                                  E03
     C                   Endif                                                  E02
     C                   Endif                                                  E01

      *          ------------------------------------------
      *          Incident description
      *          ------------------------------------------
     C     'INCD'        SCAN      OmH037        Sc                       99
     C     *In99         Ifeq      *ON                                          B02
     C                   Add       5             Sc
     C                   Z-Add     Sc            Ct

      *          ------------------------------------------
      *          Check if the end of file marker exists
      *          ------------------------------------------
     C     X'3A'         SCAN      OmH037:St     Eo                       99
     C     Eo            Ifle      W#Dsz
     C     *In99         Andeq     *On
     C     Eo            Sub       1             W#Dsz
     C                   Endif

      *          ------------------------------------------
      *          Do so we can leave.
      *          ------------------------------------------
     C     Sc            Dowlt     W#Dsz

      *          ------------------------------------------
      *          Read 70 positions or CR+LF combination
      *          ------------------------------------------
     C                   Z-add     Sc            St
     C     C#CrLf        SCAN      OmH037:St     St                       99

      *          ------------------------------------------
      *          Found a CRLF, check how long this line is
      *          ------------------------------------------
     C     *In99         Ifeq      *ON                                          B03
     C                   Eval      Lt=(St - Sc)

     C                   Eval      Ct = Ct + 2
     C     Ct            Ifne      St
     C                   Eval      Ct = St - 2

     C     Ct            Ifne      0
     C     A#H037(Ct)    Andne     '.'
     C                   Movea     '  '          A#H037(St)
     C                   Endif
     C                   Endif

     C                   Eval      Ct = St

      *          ------------------------------------------
      *          Line too big, we must split it at 70
      *          ------------------------------------------
     C     Lt            Ifgt      70
     C                   Z-Add     70            Lt
     C     Lt            Subst     OmH037:Sc     MtIncd
     C                   Movea     MtIncd        D#Incd
     C                   Move      *Blanks       MtIncd

     C     D#Incd(Lt)    Downe     *Blank
     C                   Sub       1             Lt
     C     Lt            Ifeq      *Zero
     C                   Z-add     70            Lt
     C                   Leave
     C                   Endif
     C                   Enddo

     C     Lt            Subst     OmH037:Sc     MtIncd
     C                   Add       Lt            Sc
     C                   Else

      *          ------------------------------------------
      *          Fill until CR+LF
      *          ------------------------------------------
     C     Lt            Ifgt      0
     C     Lt            Subst     OmH037:Sc     MtIncd
     C                   Add       Lt            Sc
     C                   Add       2             Sc

      *          ------------------------------------------
      *          No data on this line, fill an empty line
      *          ------------------------------------------
     C                   Else
     C                   Move      *Blanks       MtIncd
     C                   Add       2             Sc
     C                   Endif
     C                   Endif

      *          ------------------------------------------
      *          No CR+LF found, check how many characters left in
      *          the buffer.
      *          ------------------------------------------
     C                   Else

     C                   If        (W#Dsz - Sc) >= 70

     C                   Z-Add     70            Lt
     C     Lt            Subst     OmH037:Sc     MtIncd
     C                   Movea     MtIncd        D#Incd
     C                   Move      *Blanks       MtIncd

     C     D#Incd(Lt)    Downe     *Blank
     C                   Sub       1             Lt
     C     Lt            Ifeq      *Zero
     C                   Z-add     70            Lt
     C                   Leave
     C                   Endif
     C                   Enddo

     C     Lt            Subst     OmH037:Sc     MtIncd
     C                   Add       Lt            Sc

     C                   Else
     C                   Eval      Lt=(W#Dsz - Sc)
     C     Lt            Subst     OmH037:Sc     MtIncd
     C                   Add       Lt            Sc
     C                   Endif                                                  E03
     C                   Endif                                                  E03

      *          ------------------------------------------
      *          Write a text record
      *          ------------------------------------------
     C                   Write     OmhMtR
     C                   Add       10            MtSeq#
     C                   Move      *Blanks       MtIncd
     C                   Enddo                                                  E02
     C                   Endif                                                  E01

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
     C                   SETON                                        LR
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
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * *PSSR  - Error subroutine                                     *
      *                                                               *
      * ------------------------------------------------------------- *
     C     *PSSR         BEGSR
      *
      *          ------------------------------------------
      *          Do if error not in this routine
      *          ------------------------------------------
     C     I#PSSR        IFNE      *ON                                          B01
     C                   MOVE      *ON           I#PSSR            1
      *
      *          ------------------------------------------
      *          Call the error handler
      *          ------------------------------------------
     C                   CALL      'OMH961'
     C                   PARM                    PGM
     C                   PARM                    P#FILE           57
      *
      *          ------------------------------------------
      *          Move the messages to the queue of the caller
      *          ------------------------------------------
     C                   MOVEL     '*COMP'       P#MSTA           40
     C     P#MSTA        CAT       '*DIAG':5     P#MSTA
     C     P#MSTA        CAT       '*ESCA':5     P#MSTA
     C     P#MSTA        CAT       'PE':0        P#MSTA
     C     P#MSTA        CAT       '*INFO':3     P#MSTA
     C                   CALL      'QMHMOVPM'
     C                   PARM      *BLANKS       P#MSKC
     C                   PARM                    P#MSTA
     C                   PARM      4             P#LOB#
     C                   PARM      '*'           P#PMQC
     C                   PARM      1             P#PSC#
     C                   PARM                    P#ERR
      *
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
     C                   Z-ADD     *ZEROS        S#IX              5 0
     C                   Z-ADD     *ZEROS        SC                5 0
     C                   Z-ADD     *ZEROS        Eo                5 0
     C                   Z-ADD     *ZEROS        SP                5 0
     C                   Z-ADD     *ZEROS        CT                5 0
     C                   Z-ADD     *ZEROS        ST                5 0
     C                   Z-ADD     *ZEROS        Lt                5 0
     C                   Z-ADD     *ZEROS        CP                5 0
     C                   Z-ADD     *ZEROS        IY                5 0
     C                   Z-ADD     *ZEROS        IX                5 0
     C                   Z-ADD     *ZEROS        EL                5 0
     C                   Z-ADD     *ZEROS        LX                5 0
     C                   Z-ADD     *ZEROS        BX                5 0
     C                   Z-ADD     *ZEROS        FF                5 0
     C                   Z-ADD     *ZEROS        BF                5 0
     C                   Z-ADD     *ZEROS        LF                5 0
     C                   Z-ADD     *ZEROS        NB                5 0
     C                   Z-ADD     *ZEROS        W#CMD#            5 0
     C                   Z-ADD     *ZEROS        MTIRRN            5 0
     C                   Z-ADD     *ZEROS        W#RRN#            5 0
      *
      *          ------------------------------------------
      *          Other Workfields
      *          ------------------------------------------
     C                   MOVE      *BLANKS       W#WRK           256
     C                   MOVE      *BLANKS       W#FLDC            5
     C                   MOVE      *BLANKS       BLANKS           13
     C                   MOVE      *BLANKS       W#OL             80
     C     *Like         Define    RcvDsz        W#Dsz
      *
      *          ------------------------------------------
      *          Keyfields
      *          ------------------------------------------
     C     *Like         Define    Qls#          K#Qls#
     C     *Like         Define    Qsq#          K#Qsq#
     C     *Like         Define    Que#          K#Que#
     C     *Like         Define    Ans#          K#Ans#
      *
      *          ------------------------------------------
      *          Indicators
      *          ------------------------------------------
     C     *Like         Define    INDI          I#LR
      *
      *          ------------------------------------------
      *          Savefield definitions
      *          ------------------------------------------
     C     *Like         Define    RCVDID        S#DID
      *
      *          ------------------------------------------
      *          Parameter definitions
      *          ------------------------------------------
     C     *Like         Define    Dsid          P#Dsid
     C     *Like         Define    Indi          P#Opti
     C     *Like         Define    Indi          P#Dspi
     C     *Like         Define    HEDC          P#HEDC
     C     *Like         Define    INC#          P#INC#
     C     *Like         Define    INDI          P#OPTI
     C     *Like         Define    STAT          P#STAT

      *          ------------------------------------------
      *          Save field definitions
      *          ------------------------------------------

      *          ------------------------------------------
      *          API parameter definition
      *          ------------------------------------------
      /COPY QAPILESRC,DEFOTHER

      *          ------------------------------------------
      *          Parameter lists
      *          ------------------------------------------

      *          ------------------------------------------
      *          Check distribution
      *          ------------------------------------------
     C     $Mh037_2      Plist
     C                   Parm      RcvDid        P#DSID
     C                   Parm      '0'           P#DSPI
     C                   Parm      C#Norm        P#STAT

      *          ------------------------------------------
      *          Check distribution
      *          ------------------------------------------
     C     $Mh037_3      Plist
     C                   Parm      RcvDid        P#DSID
     C                   Parm      C#Norm        P#STAT

      *          ------------------------------------------
      *          Send message to a user
      *          ------------------------------------------
     C     $Mx966c       Plist
     C                   Parm                    HxSwtu
     C                   Parm      '0'           Indi
     C                   Parm      'MAI0001'     P#Msid            7
     C                   Parm      'OMHMSG'      P#Msgf           10
     C                   Parm                    P#Msgd          132

      *          ------------------------------------------
      *          OMINCL1 - Incident file
      *          ------------------------------------------
     C     $Nc1K1        KLIST
     C                   KFLD                    MHHEDC
     C                   KFLD                    MHINC#

      *          ------------------------------------------
      *          OMCcaL1 - Call track file
      *          ------------------------------------------
     C     $Ca1K1        KLIST
     C                   KFLD                    NcHEDC
     C                   KFLD                    NcCtn#

      *          ------------------------------------------
      *          OMQlaL1 - Question list answers
      *          ------------------------------------------
     C     $La1K1        KLIST
     C                   Kfld                    MhHedc
     C                   Kfld                    MqQls#
     C                   Kfld                    MqQsq#
     C                   Kfld                    LaQue#
     C                   Kfld                    MqAns#

     C     $La1K2        KLIST
     C                   Kfld                    MhHedc
     C                   Kfld                    MqQls#
     C                   Kfld                    MqQsq#

      *          ------------------------------------------
      *          Create a user space & resolve the address
      *          ------------------------------------------
     C                   EXSR      SRCUS
     C                   EXSR      SRRPU
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRRPU  - Retrieve pointer to user space                      *
      *                                                              *
      * This routine retrieves a pointer to a user space.            *
      *                                                              *
      * Input      - P#USPQ      User space qualifier                *
      *              PtrToUsp    Extended attribute                  *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRRPU         BEGSR

      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   CALL      'QUSPTRUS'
     C                   PARM                    P#USPQ
     C                   PARM                    PtrToUsp
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C     D#NBA#        IFNE      *ZEROS
     C***                CALL      'OMH901C'     $901C2
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      *
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCUS  - Create User Space                                   *
      *                                                              *
      * This routine creates a user space by calling the QUSCRTUS    *
      * API.                                                         *
      *                                                              *
      * Input      - P#USPQ  User space qualifier                    *
      *              P#EXAC  Extended attribute                      *
      *              P#ISZ#  Initial size                            *
      *              P#IVAC  Initial value                           *
      *              P#PAUC  Public authority code                   *
      *              P#USPD  User space description                  *
      *              P#RPLC  Replace option                          *
      *                                                              *
      * Destroys   - P#ERR   Error parameter data structure          *
      *                                                              *
      *                                                              *
      * ------------------------------------------------------------ *
     C     SRCUS         BEGSR
      *
      *          ------------------------------------------
      *          Fill the values needed.
      *          ------------------------------------------
     C                   EVAL      P#USPQ=C#USPQ
     C                   EVAL      P#EXAC='OMS  '
     C                   EVAL      P#ISZ#=32767
     C                   EVAL      P#IVAC=*BLANKS
     C                   EVAL      P#PAUC=C#ALL
     C                   EVAL      P#USPD=C#CBO
     C                   EVAL      P#RPLC='*YES '
      *
      *          ------------------------------------------
      *          Close the display application
      *          ------------------------------------------
     C                   CALL      'QUSCRTUS'
     C                   PARM                    P#USPQ
     C                   PARM                    P#EXAC
     C                   PARM                    P#ISZ#
     C                   PARM                    P#IVAC
     C                   PARM                    P#PAUC
     C                   PARM                    P#USPD
     C                   PARM                    P#RPLC
     C                   PARM                    P#ERR
      *
      *          ------------------------------------------
      *          Exit if errors.
      *          ------------------------------------------
     C     D#NBA#        IFNE      *ZEROS
     C***                CALL      'OMH901C'     $901C2
     C                   EXSR      SRTTLR
     C                   ENDIF
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
      * ------------------------------------------------------------- *
      /EJECT
      * ------------------------------------------------------------- *
      * Compile time arrays                                           *
      * ------------------------------------------------------------- *
