 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQUSIINCP)                                                     */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(*NONE)                                                      */
 /*   *$$ MSGF(QCPFMSG)                                                       */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
 /* ---------------------------------------------------------------- */
 /* Description                                                      */
 /* ---------------------------------------------------------------- */
 /*                                                                  */
 /* Command ....: OMQUSIIN                                           */
 /* Function ...: Interface with Incident through API                */
 /* Author .....: Remain Software                                    */
 /* Module .....: HelpDesk                                           */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Interface with Incident')

          /* ------------------------------------------------------- */
          /* Action code                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(ACTC) TYPE(*CHAR) LEN(2) RSTD(*YES) +
                          SPCVAL((*ADD 60) (*CHANGE 20) (*CLOSE 99) +
                          (*COPY 30) (*DELETE 40) (*DISPLAY 50)) +
                          MIN(1) PROMPT('Action code')

          /* ------------------------------------------------------- */
          /* Use function keys                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(FUNKEY) TYPE(*CHAR) LEN(5) RSTD(*YES) +
                          DFT(*DFT) VALUES(*DFT *USER) +
                          PMTCTL(NO_CLOSE) PROMPT('Function key +
                          definition')

          /* ------------------------------------------------------- */
          /* Function keys.                                          */
          /* ------------------------------------------------------- */
             PARM       KWD(USERKEYS) TYPE(USERKEY) MAX(23) +
                          PMTCTL(USER_KEYS) PROMPT('Function keys')

          /* ------------------------------------------------------- */
          /* Close API.                                              */
          /* ------------------------------------------------------- */
             PARM       KWD(CLOSE) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES '1') (*NO '0')) +
                          PMTCTL(NO_CLOSE) PROMPT('Close API')

          /* ------------------------------------------------------- */
          /* Re-Display option                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(REDISPLAY) TYPE(*LGL) LEN(1) RSTD(*YES) +
                          DFT(*NO) SPCVAL((*NO '0') (*YES '1')) +
                          PMTCTL(NO_CLOSE) PROMPT('Fast re-display')

          /* ------------------------------------------------------- */
          /* Protect HelpDesk                                        */
          /* ------------------------------------------------------- */
             PARM       KWD(PRDESK) TYPE(*LGL) LEN(1) RSTD(*YES) +
                          DFT(*NO) SPCVAL((*NO '0') (*YES '1')) +
                          PMTCTL(NO_CLO_DEL) PROMPT('Protect +
                          helpdesk')

          /* ------------------------------------------------------- */
          /* Use buffer for pre-fill, display only if add or change  */
          /* ------------------------------------------------------- */
             PARM       KWD(PREFILL) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NO) SPCVAL((*NO '0') (*YES '1') +
                          (*FIELD '2')) PMTCTL(ADD_CHANGE) +
                          PROMPT('Prefill fields')

          /* ------------------------------------------------------- */
          /* Cursor field, display only if PREFILL=*FIELD            */
          /* ------------------------------------------------------- */
             PARM       KWD(CRSFIELD) TYPE(*CHAR) LEN(10) +
                          PMTCTL(FIELD_FILL) PROMPT('Cursor field')

          /* ------------------------------------------------------- */
          /* The key, display only if not in add mode.               */
          /* ------------------------------------------------------- */
             PARM       KWD(KEY) TYPE(INCKEY) MAX(1) +
                          PMTCTL(NO_ADD) PROMPT('Key')

          /* ------------------------------------------------------- */
          /* Call track number                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(CALL) TYPE(*CHAR) LEN(10) PMTCTL(ADD) +
                          PROMPT('Call track number')

          /* ------------------------------------------------------- */
          /* Helpdesk code.                                          */
          /* ------------------------------------------------------- */
             PARM       KWD(DESK) TYPE(*CHAR) LEN(5) DFT(*CURRENT) +
                          SPCVAL((*CURRENT *CUR)) CHOICE('Name, +
                          *CURRENT') PMTCTL(USE_BUF_NK) +
                          PROMPT('Helpdesk')

          /* ------------------------------------------------------- */
          /* Incident                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(INC) TYPE(*CHAR) LEN(10) DFT(*GEN) +
                          CHOICE('Incident number') +
                          PMTCTL(USE_BUF_NK) PROMPT('Incident')

          /* ------------------------------------------------------- */
          /* Short incident description                              */
          /* ------------------------------------------------------- */
             PARM       KWD(SHID) TYPE(*CHAR) LEN(50) DFT(*SAME) +
                          SPCVAL((*SAME ' ')) PMTCTL(USE_BUFFER) +
                          PROMPT('Short description')

          /* ------------------------------------------------------- */
          /* Accumulated incident                                    */
          /* ------------------------------------------------------- */
             PARM       KWD(ACCU) TYPE(*CHAR) LEN(10) CONSTANT(' ')

          /* ------------------------------------------------------- */
          /* Income date                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(INDT) TYPE(*DATE) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Income time                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(INTM) TYPE(*TIME) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Realized completion date                                */
          /* ------------------------------------------------------- */
             PARM       KWD(RCDT) TYPE(*DATE) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Realized completion time                                */
          /* ------------------------------------------------------- */
             PARM       KWD(RCTM) TYPE(*TIME) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Status route                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(ROUT) TYPE(*CHAR) LEN(10) DFT(*SAME) +
                          SPCVAL((*FIRST)) PMTCTL(USE_BUFFER) +
                          PROMPT('Status route')

          /* ------------------------------------------------------- */
          /* Status route sequence                                   */
          /* ------------------------------------------------------- */
             PARM       KWD(RSEQ) TYPE(*DEC) LEN(3 0) DFT(*SAME) +
                          SPCVAL((*SAME -1) (*FIRST -2) (*NEXT -3)) +
                          PMTCTL(USE_BUFFER) PROMPT('Status route +
                          sequence')

          /* ------------------------------------------------------- */
          /* Support level                                           */
          /* ------------------------------------------------------- */
             PARM       KWD(SUL) TYPE(*CHAR) LEN(1) CONSTANT('0')

          /* ------------------------------------------------------- */
          /* Status                                                  */
          /* ------------------------------------------------------- */
             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')

          /* ------------------------------------------------------- */
          /* Status date                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(STDT) TYPE(*DATE) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Status time                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(STTM) TYPE(*TIME) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Technical severity                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV1) TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(USE_BUFFER) +
                          PROMPT('Technical severity')

          /* ------------------------------------------------------- */
          /* Customer severity                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV2) TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(USE_BUFFER) +
                          PROMPT('Customer severity')

          /* ------------------------------------------------------- */
          /* Marketing severity                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV3) TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(USE_BUFFER) +
                          PROMPT('Marketing severity')

          /* ------------------------------------------------------- */
          /* Priority                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(PRI) TYPE(*DEC) LEN(3 0) CONSTANT(0)

          /* ------------------------------------------------------- */
          /* Escalation counter                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(ESC) TYPE(*DEC) LEN(3 0) CONSTANT(0)

          /* ------------------------------------------------------- */
          /* Incident escalation date                                */
          /* ------------------------------------------------------- */
             PARM       KWD(IEDT) TYPE(*DATE) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Incident escalation time                                */
          /* ------------------------------------------------------- */
             PARM       KWD(IETM) TYPE(*TIME) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Registered by user                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(REGU) TYPE(*CHAR) LEN(10) CONSTANT(' ')

          /* ------------------------------------------------------- */
          /* Allocated by user                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(ALOC) TYPE(*CHAR) LEN(10) CONSTANT(' ')

          /* ------------------------------------------------------- */
          /* Incident type                                           */
          /* ------------------------------------------------------- */
             PARM       KWD(ITYP) TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(USE_BUFFER) +
                          PROMPT('Incident type')

          /* ------------------------------------------------------- */
          /* Configuration list                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(CLST) TYPE(*DEC) LEN(5 0) CONSTANT(000)

          /* ------------------------------------------------------- */
          /* Configuration item                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(CITM) TYPE(*CHAR) LEN(12) CONSTANT(' ')

          /* ------------------------------------------------------- */
          /* User field 1                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF1) TYPE(*CHAR) LEN(10) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(USE_BUFFER) +
                          PROMPT('User field 1')

          /* ------------------------------------------------------- */
          /* User field 2                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF2) TYPE(*CHAR) LEN(25) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(USE_BUFFER) +
                          PROMPT('User field 2')

          /* ------------------------------------------------------- */
          /* User field 3                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF3) TYPE(*DEC) LEN(15 0) DFT(*SAME) +
                          SPCVAL((*SAME -1)) PMTCTL(USE_BUFFER) +
                          PROMPT('User field 3')

 /* ---------------------------------------------------------------- */
 /* Elements of composit keywords                                    */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* The incident key                                        */
          /*                                                         */
          /* HelpDesk                                                */
          /* Incident                                                */
          /* ------------------------------------------------------- */

 INCKEY:     ELEM       TYPE(*CHAR) LEN(5) CHOICE('Name') +
                          PROMPT('HelpDesk')
             ELEM       TYPE(*CHAR) LEN(10) MIN(1) CHOICE('Number') +
                          PROMPT('Incident')

          /* ------------------------------------------------------- */
          /* The function keys.                                      */
          /*                                                         */
          /* function key number                                     */
          /* Message id                                              */
          /* Message file qualifier                                  */
          /* Function key text                                       */
          /* ------------------------------------------------------- */

 USERKEY:    ELEM       TYPE(*INT2) RANGE(2 24) PROMPT('Function key +
                          number')

             ELEM       TYPE(*CHAR) LEN(1) RSTD(*YES) DFT(*NO) +
                          SPCVAL((*YES 1) (*NO 0)) PROMPT('Process +
                          screen data')

             ELEM       TYPE(*NAME) LEN(7) DFT(*NONE) SPCVAL((*NONE +
                          *NONE)) CHOICE('Message id') +
                          PROMPT('Message identifier')

             ELEM       TYPE(MSGQUAL) MIN(1) CHOICE('Name') +
                          PROMPT('Message file')

             ELEM       TYPE(*CHAR) LEN(30) DFT(*BLANK) +
                          CHOICE('Text') PROMPT('Or function key text')

 /* ---------------------------------------------------------------- */
 /* Qualifiers                                                       */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Message file qualifier                                  */
          /* ------------------------------------------------------- */
 MSGQUAL:    QUAL       TYPE(*NAME) LEN(10) DFT(OMHDSP)
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL((*LIBL +
                          *LIBL)) PROMPT('Library')

 /* ---------------------------------------------------------------- */
 /* Prompt controls                                                  */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Action code is not *ADD.                                */
          /* ------------------------------------------------------- */
 NO_ADD:     PMTCTL     CTL(ACTC) COND((*NE 60))
             PMTCTL     CTL(ACTC) COND((*NE 99)) LGLREL(*AND)

 ADD:        PMTCTL     CTL(ACTC) COND((*EQ 60))

 ADD_CHANGE: PMTCTL     CTL(ACTC) COND((*EQ 20))
             PMTCTL     CTL(ACTC) COND((*EQ 30)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 60)) LGLREL(*OR)

          /* ------------------------------------------------------- */
          /* Action code is not *CLOSE, *DELETE or *DISPLAY          */
          /* ------------------------------------------------------- */
 NO_CLO_DEL: PMTCTL     CTL(ACTC) COND((*NE 99))
             PMTCTL     CTL(ACTC) COND((*NE 40))
             PMTCTL     CTL(ACTC) COND((*NE 50))

          /* ------------------------------------------------------- */
          /* Action code is not *CLOSE                               */
          /* ------------------------------------------------------- */
 NO_CLOSE:   PMTCTL     CTL(ACTC) COND((*NE 99))

          /* ------------------------------------------------------- */
          /* No change will occur in the API when the action is      */
          /* not change or add. It is then useless to fill the       */
          /* buffer fields.                                          */
          /* ------------------------------------------------------- */
 CHANGE:     PMTCTL     CTL(ACTC) COND((*EQ 20))

          /* ------------------------------------------------------- */
          /* User function keys                                      */
          /* ------------------------------------------------------- */

 USER_KEYS:  PMTCTL     CTL(FUNKEY) COND((*EQ '*USER'))

          /* ------------------------------------------------------- */
          /* Use buffer is set                                       */
          /* ------------------------------------------------------- */

 USE_BUF_NK: PMTCTL     CTL(PREFILL) COND((*NE '0'))
             PMTCTL     CTL(ACTC) COND((*NE 20))

 USE_BUFFER: PMTCTL     CTL(PREFILL) COND((*NE '0'))

 FIELD_FILL: PMTCTL     CTL(PREFILL) COND((*EQ '2'))
