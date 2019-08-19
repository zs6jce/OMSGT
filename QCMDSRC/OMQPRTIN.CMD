 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQPRTINCP)                                                     */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(OMHDSP)                                                     */
 /*   *$$ MSGF(QCPFMSG)                                                       */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
 /* ---------------------------------------------------------------- */
 /* Description                                                      */
 /* ---------------------------------------------------------------- */
 /*                                                                  */
 /* Command ....: OMQPRTIN                                           */
 /* Function ...: Print incident through API                         */
 /* Author .....: Remain Software                                    */
 /*                                                                  */
 /* Remark .....: Default values can not be changed.                 */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Print incident by API')

          /* ------------------------------------------------------- */
          /* Action code                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(ACTC) TYPE(*CHAR) LEN(2) RSTD(*YES) +
                          DFT(*PRINT) SPCVAL((*PRINT 00) (*CLOSE +
                          99)) PROMPT(MQP0001 1) /* Action Code */

          /* ------------------------------------------------------- */
          /* Helpdesk code.                                          */
          /* ------------------------------------------------------- */
             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE('*ALL, code') +
                          PMTCTL(KEYL) PROMPT(MQP0002 2) /* Help +
                          Desk */

          /* ------------------------------------------------------- */
          /* Incident                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(INC) TYPE(*CHAR) LEN(10) DFT(*OPEN) +
                          SPCVAL((*ALL) (*OPEN) (*COMPLETE *CMP) +
                          (*NOOMS)) +
                          PMTCTL(KEYL) PROMPT(MQP0003 3) /* +
                          Incident number */

          /* ------------------------------------------------------- */
          /* Call track number                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(CTN) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) PMTCTL(SLCT) +
                          PROMPT(MQP0004 4) /* Call track number */

          /* ------------------------------------------------------- */
          /* Customer code                                           */
          /* ------------------------------------------------------- */
             PARM       KWD(CUSC) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) PMTCTL(SLCT) +
                          PROMPT(CUS0001 4) /* Customer code */

          /* ------------------------------------------------------- */
          /* Contact code                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(CCTC) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) PMTCTL(SLCT) +
                          PROMPT(CNT0001 4) /* Contact code */

          /* ------------------------------------------------------- */
          /* Income date                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(INDT) TYPE(DAT1) PMTCTL(SLCT) +
                          PROMPT(MQP0005) /* Incoming date */

 DAT1:       ELEM       TYPE(*DATE) DFT(*BEGIN) SPCVAL((*CURRENT +
                          5555555) (*BEGIN 0000000) (*END 9999999)) +
                          CHOICE(MQP0008) PROMPT(MQP0006) /* From */

             ELEM       TYPE(*DATE) DFT(*CURRENT) SPCVAL((*CURRENT +
                          5555555) (*BEGIN 0000000) (*END 9999999)) +
                          CHOICE(MQP0009) PROMPT(MQP0007) /* To */

          /* ------------------------------------------------------- */
          /* Income time                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(INTM) TYPE(TIM) PMTCTL(SLCT) +
                          PROMPT(MQP0010) /* Incoming time */

 TIM:        ELEM       TYPE(*TIME) DFT(*AVAIL) SPCVAL((*AVAIL +
                           000000)) CHOICE(MQP0013) PROMPT(MQP0011)

             ELEM       TYPE(*TIME) DFT(*AVAIL) SPCVAL((*AVAIL +
                           235959)) CHOICE(MQP0013) PROMPT(MQP0012) +
                          /* To */

          /* ------------------------------------------------------- */
          /* Realized completion date                                */
          /* ------------------------------------------------------- */
             PARM       KWD(RCDT) TYPE(DAT2) PMTCTL(SLCT) +
                          PROMPT(MQP0042) /* Realization date */

 DAT2:       ELEM       TYPE(*DATE) DFT(*BEGIN) SPCVAL((*CURRENT +
                          5555555) (*BEGIN 0000000) (*END 9999999)) +
                          CHOICE(MQP0008) PROMPT(MQP0006) /* From */

             ELEM       TYPE(*DATE) DFT(*CURRENT) SPCVAL((*CURRENT +
                          5555555) (*BEGIN 0000000) (*END 9999999)) +
                          CHOICE(MQP0009) PROMPT(MQP0007) /* To */

          /* ------------------------------------------------------- */
          /* Status route                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(STRC) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0015) /* Status +
                          route code */

          /* ------------------------------------------------------- */
          /* Support level                                           */
          /* ------------------------------------------------------- */
             PARM       KWD(SULC) TYPE(*CHAR) LEN(4) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0030) +
                          PMTCTL(SLCT) PROMPT(MQP0016) /* Support +
                          level */

          /* ------------------------------------------------------- */
          /* Status                                                  */
          /* ------------------------------------------------------- */
             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0017) /* Status */

          /* ------------------------------------------------------- */
          /* Technical severity                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV1) TYPE(*CHAR) LEN(5) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0018) /* Severity 1 */

          /* ------------------------------------------------------- */
          /* Customer severity                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV2) TYPE(*CHAR) LEN(5) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0019) /* Severity 2 */

          /* ------------------------------------------------------- */
          /* Marketing severity                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV3) TYPE(*CHAR) LEN(5) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0020) /* Severity 3 */

          /* ------------------------------------------------------- */
          /* Priority                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(PRIC) TYPE(PRI) PMTCTL(SLCT) +
                          PROMPT(MQP0021) /* Priority */

PRI:         ELEM       TYPE(*DEC) LEN(3 0) DFT(0) RANGE(0 999) +
                          CHOICE(*VALUES) PROMPT('Low') /* Priority +
                          value */

             ELEM       TYPE(*DEC) LEN(3 0) DFT(999) RANGE(0 999) +
                          CHOICE(*VALUES) PROMPT('High') /* +
                          Priority value */

          /* ------------------------------------------------------- */
          /* Registered by user                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(RUID) TYPE(*NAME) LEN(10) DFT(*ALL) +
                          SPCVAL((*USER) (*ALL)) CHOICE(MQP0028) +
                          PMTCTL(SLCT) PROMPT(MQP0022) /* +
                          Registered by */

          /* ------------------------------------------------------- */
          /* Allocated by user                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(AUID) TYPE(*NAME) LEN(10) DFT(*ALL) +
                          SPCVAL((*USER) (*ANY) (*ALL) (*NONE)) +
                          PMTCTL(SLCT) PROMPT(MQP0023) /* Allocated +
                          by */

          /* ------------------------------------------------------- */
          /* Incident type                                           */
          /* ------------------------------------------------------- */
             PARM       KWD(ITYP) TYPE(*CHAR) LEN(5) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0024) /* Incident +
                          type code */

          /* ------------------------------------------------------- */
          /* User field 1                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF1) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0025) /* User +
                          field 1 */

          /* ------------------------------------------------------- */
          /* User field 2                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF2) TYPE(*CHAR) LEN(25) DFT(*ALL) +
                          SPCVAL((*ALL)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0026) /* User +
                          field 2 */

          /* ------------------------------------------------------- */
          /* User field 3                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF3) TYPE(*DEC) LEN(15 0) DFT(*ALL) +
                          SPCVAL((*ALL -1)) CHOICE(MQP0014) +
                          PMTCTL(SLCT) PROMPT(MQP0027) /* User +
                          field 3 */

          /* ------------------------------------------------------- */
          /* Escalation count                                        */
          /* ------------------------------------------------------- */
             PARM       KWD(ESC) TYPE(ESC) PMTCTL(SLCT) +
                          PROMPT(MQP0047) /* Escalation counter */

 ESC:        ELEM       TYPE(*CHAR) LEN(3) RSTD(*YES) DFT(*GE) +
                          VALUES(*LT *LE *GT *GE *EQ) +
                          CHOICE(*VALUES) PROMPT(MQP0048) /* +
                          Operator */

             ELEM       TYPE(*DEC) LEN(3 0) DFT(0) CHOICE(*VALUES) +
                          PROMPT(MQP0049) /* Escalation value */

          /* ------------------------------------------------------- */
          /* Escalation date                                         */
          /* ------------------------------------------------------- */
             PARM       KWD(ESDT) TYPE(*DATE) DFT(*BEGIN) +
                          SPCVAL((*BEGIN 0000000)) CHOICE(MQP0050) +
                          PMTCTL(SLCT) PROMPT(MQP0051) /* +
                          Escalation date */

          /* ------------------------------------------------------- */
          /* Answer list                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(ANSL) TYPE(*CHAR) LEN(10) DFT(*NONE) +
                          SPCVAL((*NONE)) CHOICE(MQP0054) +
                          PMTCTL(SLCT) PROMPT(MQP0055) /* Answer +
                          list */

          /* ------------------------------------------------------- */
          /* Print question and answer history                       */
          /* ------------------------------------------------------- */
             PARM       KWD(QUAN) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NO) SPCVAL((*NO 0) (*YES 1)) +
                          PMTCTL(SLCT) PROMPT(MQP0029) /* +
                          Question/answer history */

          /* ------------------------------------------------------- */
          /* Search history                                          */
          /* ------------------------------------------------------- */
             PARM       KWD(HIST) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NO) SPCVAL((*NO 0) (*YES 1)) +
                          PROMPT(MQP0076) /* search history */

          /* ------------------------------------------------------- */
          /* Average                                                 */
          /* ------------------------------------------------------- */
             PARM       KWD(PAVE) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NO) SPCVAL((*NO 0) (*YES 1)) +
                          PROMPT(MQP0066) /* Print +
                          Average */

          /* ------------------------------------------------------- */
          /* To status only when average is *YES                     */
          /* ------------------------------------------------------- */
             PARM       KWD(TSTA) TYPE(STA) PMTCTL(TSTA) +
                          PROMPT(MQP0071)

 STA:        ELEM       TYPE(*CHAR) LEN(10) DFT(*FIRST) +
                          SPCVAL((*FIRST 0) (*ALL 1)) +
                          CHOICE(MQP0072) PROMPT(MQP0067)

             ELEM       TYPE(*CHAR) LEN(5) DFT(*FIRST) +
                          SPCVAL((*FIRST 0)) CHOICE(MQP0073) +
                          PROMPT(MQP0068)

             ELEM       TYPE(*CHAR) LEN(10) DFT(*NONE) SPCVAL((*NONE +
                          0) (*LAST 1)) CHOICE(MQP0074) PROMPT(MQP0069)

             ELEM       TYPE(*CHAR) LEN(5) DFT(*NONE) SPCVAL((*NONE +
                          0) (*COMPLETE 1)) CHOICE(MQP0075) +
                          PROMPT(MQP0070)

          /* ------------------------------------------------------- */
          /* Print incident relationships.                           */
          /* ------------------------------------------------------- */
             PARM       KWD(IIRI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NONE) SPCVAL((*NONE '0') (*SHORT +
                          '1')) CHOICE(MQP0031) PMTCTL(SLCT) +
                          PROMPT(MQP0035) /* Incident relationships */

          /* ------------------------------------------------------- */
          /* Print request relationships.                            */
          /* ------------------------------------------------------- */
             PARM       KWD(REQR) TYPE(*CHAR) LEN(1) DFT(*NONE) +
                          SPCVAL((*NONE '0') (*SHORT '1')) +
                          CHOICE(MQP0031) PMTCTL(SLCT) +
                          PROMPT(MQP0034) /* Request  relationships */

          /* ------------------------------------------------------- */
          /* Print fix relationships                                 */
          /* ------------------------------------------------------- */
             PARM       KWD(FIXR) TYPE(*CHAR) LEN(1) DFT(*NONE) +
                          SPCVAL((*NONE '0') (*SHORT '1')) +
                          CHOICE(MQP0031) PMTCTL(SLCT) +
                          PROMPT(MQP0052) /* Fix  relationships */

          /* ------------------------------------------------------- */
          /* Print solution relationships                            */
          /* ------------------------------------------------------- */
             PARM       KWD(SOLR) TYPE(*CHAR) LEN(1) DFT(*NONE) +
                          SPCVAL((*NONE '0') (*SHORT '1')) +
                          CHOICE(MQP0031) PMTCTL(SLCT) +
                          PROMPT(MQP0053) /* Sol. relationships */

          /* ------------------------------------------------------- */
          /* List of detail incident print.                          */
          /* ------------------------------------------------------- */
             PARM       KWD(LODI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*SHORT) SPCVAL((*SHORT '0') (*DETAIL +
                          '1')) PMTCTL(SLCT) PROMPT(MQP0033) /* +
                          Output format */

          /* ------------------------------------------------------- */
          /* Print text                                              */
          /* ------------------------------------------------------- */
             PARM       KWD(PITI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NONE) SPCVAL((*NONE '0') (*SYMPTOM +
                          '1') (*CAUSE '2') (*SOLUTION '3') (*OTHER +
                          '4') (*ALL '9')) PMTCTL(SLCT) +
                          PROMPT(MQP0057) /* Print incident text */

          /* ------------------------------------------------------- */
          /* Print incident per page                                 */
          /* ------------------------------------------------------- */
             PARM       KWD(PIPI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES '1') (*NO '0')) +
                          PMTCTL(SLCT) PROMPT(MQP0037) /* Incident +
                          per page */

          /* ------------------------------------------------------- */
          /* Print cover page                                        */
          /* ------------------------------------------------------- */
             PARM       KWD(PCPI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES '1') (*NO '0')) +
                          PMTCTL(SLCT) PROMPT(MQP0038) /* Print +
                          cover page */

          /* ------------------------------------------------------- */
          /* Print end page.                                         */
          /* ------------------------------------------------------- */
             PARM       KWD(PEPI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES '1') (*NO '0')) +
                          PMTCTL(SLCT) PROMPT(MQP0039) /* Print end +
                          page */

          /* ------------------------------------------------------- */
          /* Close API                                               */
          /* ------------------------------------------------------- */
             PARM       KWD(CLPI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES '1') (*NO '0')) +
                          PMTCTL(SLCT) PROMPT(MQP0056) /* Close prt +
                          file indicator */

          /* ------------------------------------------------------- */
          /* Close API                                               */
          /* ------------------------------------------------------- */
             PARM       KWD(EOPI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES '1') (*NO '0')) +
                          PMTCTL(SLCT) PROMPT(MQP0040) /* Close API +
                          indicator */

          /* ------------------------------------------------------- */
          /* Prompt controls                                         */
          /* ------------------------------------------------------- */
 KEYL:       PMTCTL     CTL(ACTC) COND((*NE 99))
 SLCT:       PMTCTL     CTL(HEDC) COND((*NE ' '))
 HEDA:       PMTCTL     CTL(HEDC) COND((*EQ '*ALL'))
 INCA:       PMTCTL     CTL(INC) COND((*EQ '*ALL'))
 TSTA:       PMTCTL     CTL(PAVE) COND((*EQ '1'))
