 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQCHGINCP)                                                     */
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
 /* Command ....: OMQCHGIN                                           */
 /* Function ...: Change Incident through API                        */
 /* Author .....: Remain Software                                    */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Change Incident through API')

          /* ------------------------------------------------------- */
          /* Action code                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(ACTC) TYPE(*CHAR) LEN(2) RSTD(*YES) +
                          SPCVAL((*ACCUMULATE 25) (*ADD 60) +
                          (*ALLOCATE 15) (*CHANGE 20) (*CLOSE 99) +
                          (*COMPLETE 42) (*CONNECT 23) (*COPY 30) +
                          (*DEALLOCATE 85) (*DISCONNECT 27) +
                          (*ESCALATE 33) (*OMS 87) (*REOPEN 09) +
                          (*STATUS 29)) MIN(1) PROMPT('Action code' 1)

          /* ------------------------------------------------------- */
          /* HelpDesk                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(DESK) TYPE(*CHAR) LEN(5) PMTCTL(KEYL) +
                          PROMPT('HelpDesk' 3)

          /* ------------------------------------------------------- */
          /* Incident                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(INC) TYPE(*CHAR) LEN(10) SPCVAL((*GEN)) +
                          PMTCTL(KEYL) PROMPT('Incident' 4)

          /* ------------------------------------------------------- */
          /* Call track number                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(CALL) TYPE(*CHAR) LEN(10) PMTCTL(CPYADD) +
                          PROMPT('Call track number' 5)

          /* ------------------------------------------------------- */
          /* Short incident description                              */
          /* ------------------------------------------------------- */
             PARM       KWD(SHID) TYPE(*CHAR) LEN(50) DFT(*SAME) +
                          SPCVAL((*SAME ' ')) PMTCTL(CHG) +
                          PROMPT('Short description')

          /* ------------------------------------------------------- */
          /* Accumulated incident                                    */
          /* ------------------------------------------------------- */
             PARM       KWD(ACCU) TYPE(*CHAR) LEN(10) DFT(*SAME) +
                          PMTCTL(ACCU) PROMPT('Accumulated incident')

          /* ------------------------------------------------------- */
          /* Income date                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(INDT) TYPE(*DATE) DFT(*CURRENT) +
                          SPCVAL((*CURRENT 0000000)) PMTCTL(ADD) +
                          PROMPT('Income date')

          /* ------------------------------------------------------- */
          /* Income time                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(INTM) TYPE(*TIME) DFT(*CURRENT) +
                          SPCVAL((*CURRENT 000000)) PMTCTL(ADD) +
                          PROMPT('Income time')

          /* ------------------------------------------------------- */
          /* Realized completion date                                */
          /* ------------------------------------------------------- */
             PARM       KWD(RCDT) TYPE(*DATE) CONSTANT(0000000)

          /* ------------------------------------------------------- */
          /* Realized completion time                                */
          /* ------------------------------------------------------- */
             PARM       KWD(RCTM) TYPE(*TIME) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Status route                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(ROUT) TYPE(*CHAR) LEN(10) DFT(*SAME) +
                          SPCVAL((*FIRST)) PMTCTL(STAT) +
                          PROMPT('Status route')

          /* ------------------------------------------------------- */
          /* Status route sequence                                   */
          /* ------------------------------------------------------- */
             PARM       KWD(RSEQ) TYPE(*DEC) LEN(3 0) DFT(*SAME) +
                          SPCVAL((*SAME -1) (*FIRST -2) (*NEXT -3)) +
                          PMTCTL(STAT) PROMPT('Status route sequence')

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
             PARM       KWD(STDT) TYPE(*DATE) CONSTANT(0000000)

          /* ------------------------------------------------------- */
          /* Status time                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(STTM) TYPE(*TIME) CONSTANT(000000)

          /* ------------------------------------------------------- */
          /* Technical severity                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV1)  TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME ) (*DFT)) +
                          PMTCTL(CHG) PROMPT('Technical severity')

          /* ------------------------------------------------------- */
          /* Customer severity                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV2)  TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME ) (*DFT)) +
                          PMTCTL(CHG) PROMPT('Customer severity')

          /* ------------------------------------------------------- */
          /* Marketing severity                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV3)  TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME ) (*DFT)) +
                          PMTCTL(CHG) PROMPT('Marketing severity')

          /* ------------------------------------------------------- */
          /* Priority                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(PRI) TYPE(*DEC) LEN(3 0) DFT(*SAME) +
                          SPCVAL((*SAME 0)) PMTCTL(ESCA) +
                          PROMPT('Priority' 5)

          /* ------------------------------------------------------- */
          /* Escalation counter                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(ESC) TYPE(*DEC) LEN(3 0) DFT(*SAME) +
                          SPCVAL((*SAME 0)) PMTCTL(ESCA) +
                          PROMPT('Escalation counter' 7)

          /* ------------------------------------------------------- */
          /* Escalation date                                         */
          /* ------------------------------------------------------- */
             PARM       KWD(IEDT) TYPE(*DATE) RSTD(*YES) DFT(*SAME) +
                          SPCVAL((*SAME 9999999) (*CURRENT 8888888)) +
                          PMTCTL(ESCA) PROMPT('Escalation date' 9)

          /* ------------------------------------------------------- */
          /* Escalation time                                         */
          /* ------------------------------------------------------- */
             PARM       KWD(IETM) TYPE(*TIME) DFT(*SAME) +
                          SPCVAL((*SAME 999999) (*CURRENT 888888)) +
                          PMTCTL(ESCA) PROMPT('Escalation time' 10)

          /* ------------------------------------------------------- */
          /* Registered by user                                      */
          /* ------------------------------------------------------- */
             PARM       KWD(REGU) TYPE(*CHAR) LEN(10) CONSTANT(' ')

          /* ------------------------------------------------------- */
          /* Allocated by user                                       */
          /* ------------------------------------------------------- */
             PARM       KWD(ALOC) TYPE(*NAME) LEN(10) DFT(*CURRENT) +
                          SPCVAL((*CURRENT)) PMTCTL(ALC) +
                          PROMPT('Allocated by user')

          /* ------------------------------------------------------- */
          /* Incident type                                           */
          /* ------------------------------------------------------- */
             PARM       KWD(ITYP) TYPE(*CHAR) LEN(5) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(CHG) +
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
                          SPCVAL((*SAME)) PMTCTL(CHG) +
                          PROMPT('User field 1')

          /* ------------------------------------------------------- */
          /* User field 2                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF2) TYPE(*CHAR) LEN(25) DFT(*SAME) +
                          SPCVAL((*SAME)) PMTCTL(CHG) +
                          PROMPT('User field 2')

          /* ------------------------------------------------------- */
          /* User field 3                                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF3) TYPE(*DEC) LEN(15 0) DFT(*SAME) +
                          SPCVAL((*SAME -1)) PMTCTL(CHG) +
                          PROMPT('User field 3')

          /* ------------------------------------------------------- */
          /* To helpdesk                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(TDSK) TYPE(*CHAR) LEN(5) PMTCTL(CPY) +
                          PROMPT('To HelpDesk')

          /* ------------------------------------------------------- */
          /* To incident                                             */
          /* ------------------------------------------------------- */
             PARM       KWD(TINC) TYPE(*CHAR) LEN(10) +
                          SPCVAL((*GEN)) PMTCTL(CPY) +
                          PROMPT('To incident')

          /* ------------------------------------------------------- */
          /* To Application                                          */
          /* ------------------------------------------------------- */
             PARM       KWD(TAPP) TYPE(*CHAR) LEN(5) PMTCTL(OMS) +
                          PROMPT('To Application')

          /* ------------------------------------------------------- */
          /* To Request                                              */
          /* ------------------------------------------------------- */
             PARM       KWD(TREQ) TYPE(*CHAR) LEN(10) +
                          SPCVAL((*GEN)) PMTCTL(OMS) +
                          PROMPT('To Request')

 /* ---------------------------------------------------------------- */
 /* API control codes                                                */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Close indicator                                         */
          /* ------------------------------------------------------- */
             PARM       KWD(EOPI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*YES) SPCVAL((*YES 1) (*NO 0)) +
                          PMTCTL(KEYL) PROMPT('Close API indicator')

          /* ------------------------------------------------------- */
          /* Update database indicator                               */
          /* The parameter is not included because there are too     */
          /* many parameters to pass to OMQCHGINCP                   */
          /* ------------------------------------------------------- */
          /* PARM       KWD(UPDI) TYPE(*CHAR) LEN(1) CONSTANT(1)     */

          /* ------------------------------------------------------- */
          /* Update buffer indicator                                 */
          /* The parameter is not included because there are too     */
          /* many parameters to pass to OMQCHGINCP                   */
          /* ------------------------------------------------------- */
          /* PARM       KWD(UPBI) TYPE(*CHAR) LEN(1) CONSTANT(1)     */

          /* ------------------------------------------------------- */
          /* Add, subtract or absolute for priority                  */
          /* ------------------------------------------------------- */
             PARM       KWD(PCMI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*ADD) SPCVAL((*ADD '1') (*SUB '2') +
                          (*ABS '3')) PMTCTL(ESCA) +
                          PROMPT('Priority calculation method' 6)

          /* ------------------------------------------------------- */
          /* Add, subtract or absolute for counter                   */
          /* ------------------------------------------------------- */
             PARM       KWD(CCMI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*ADD) SPCVAL((*ADD '1') (*SUB '2') +
                          (*ABS '3')) PMTCTL(ESCA) +
                          PROMPT('Counter calculation method' 8)

          /* ------------------------------------------------------- */
          /* Request connection method                               */
          /* ------------------------------------------------------- */
             PARM       KWD(RCMI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*CONNECT) SPCVAL((*CONNECT '0') +
                          (*DISCONNECT '1')) PMTCTL(OMSACCU) +
                          PROMPT('Request connection method' 9)

 /* ---------------------------------------------------------------- */
 /* Prompt controls                                                  */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Key                                                     */
          /* ------------------------------------------------------- */
 KEYL:       PMTCTL     CTL(ACTC) COND((*EQ 09))
             PMTCTL     CTL(ACTC) COND((*EQ 15)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 20)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 23)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 25)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 27)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 29)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 30)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 33)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 40)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 42)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 60)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 70)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 85)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 87)) LGLREL(*OR)

          /* ------------------------------------------------------- */
          /* Escalate                                                */
          /* ------------------------------------------------------- */
 ESCA:       PMTCTL     CTL(ACTC) COND((*EQ 33))

          /* ------------------------------------------------------- */
          /* Add.                                                    */
          /* ------------------------------------------------------- */
 ADD:        PMTCTL     CTL(ACTC) COND((*EQ 60))

          /* ------------------------------------------------------- */
          /* Copy & add for the call track number.                   */
          /* ------------------------------------------------------- */
 CPYADD:     PMTCTL     CTL(ACTC) COND((*EQ 60))
             PMTCTL     CTL(ACTC) COND((*EQ 30)) LGLREL(*OR)

          /* ------------------------------------------------------- */
          /* Change, attributes can be entered                       */
          /* ------------------------------------------------------- */
 CHG:        PMTCTL     CTL(ACTC) COND((*EQ 20))
             PMTCTL     CTL(ACTC) COND((*EQ 60)) LGLREL(*OR)

          /* ------------------------------------------------------- */
          /* Copy incident to another incident, or connect.          */
          /* ------------------------------------------------------- */
 CPY:        PMTCTL     CTL(ACTC) COND((*EQ 30))
             PMTCTL     CTL(ACTC) COND((*EQ 23)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 27)) LGLREL(*OR)

          /* ------------------------------------------------------- */
          /* Change, and status.                                     */
          /* ------------------------------------------------------- */
 STAT:       PMTCTL     CTL(ACTC) COND((*EQ 09))
             PMTCTL     CTL(ACTC) COND((*EQ 20)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 29)) LGLREL(*OR)
             PMTCTL     CTL(ACTC) COND((*EQ 60)) LGLREL(*OR)

          /* ------------------------------------------------------- */
          /* Connect to request.                                     */
          /* ------------------------------------------------------- */
 OMS:        PMTCTL     CTL(ACTC) COND((*EQ 87))

          /* ------------------------------------------------------- */
          /* Allocate incident to user.                              */
          /* ------------------------------------------------------- */
 ALC:        PMTCTL     CTL(ACTC) COND((*EQ 15))

          /* ------------------------------------------------------- */
          /* Accumulate incident to another incident                 */
          /* ------------------------------------------------------- */
 ACCU:       PMTCTL     CTL(ACTC) COND((*EQ 25))

          /* ------------------------------------------------------- */
          /* OMS or Accumulate                                       */
          /* ------------------------------------------------------- */
 OMSACCU:    PMTCTL     CTL(ACTC) COND((*EQ 25))
             PMTCTL     CTL(ACTC) COND((*EQ 87)) LGLREL(*OR)
