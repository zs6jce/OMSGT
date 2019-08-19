 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQRTVINCP)                                                     */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*BPGM *IPGM *BREXX *IREXX)                                    */
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
 /* Command ....: OMQRTVIN                                           */
 /* Function ...: Retrieve Incident by API                           */
 /* Author .....: Remain Software                                    */
 /* Module .....: Helpdesk                                           */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Retrieve Incident')

          /* ------------------------------------------------------- */
          /* Help Desk                                               */
          /* ------------------------------------------------------- */
             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) MIN(1) +
                          CHOICE('Code') PROMPT('HelpDesk')

          /* ------------------------------------------------------- */
          /* Incident                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(INC) TYPE(*CHAR) LEN(10) MIN(1) +
                          CHOICE('Incident number') PROMPT('Incident')

          /* ------------------------------------------------------- */
          /* Returnvalue for call track number                       */
          /* ------------------------------------------------------- */
             PARM       KWD(CTN) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for CTN          (10)')

          /* ------------------------------------------------------- */
          /* Returnvalue for assiciated incident number              */
          /* ------------------------------------------------------- */
             PARM       KWD(AIN) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for AIN          (10)')

          /* ------------------------------------------------------- */
          /* Returnvalue for incoming date                           */
          /* ------------------------------------------------------- */
             PARM       KWD(INDT) TYPE(*DEC) LEN(7 0) RTNVAL(*YES) +
                          PROMPT('CL var for INDT        (7 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for incoming time                           */
          /* ------------------------------------------------------- */
             PARM       KWD(INTM) TYPE(*DEC) LEN(6 0) RTNVAL(*YES) +
                          PROMPT('CL var for INTM        (6 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for completion date                         */
          /* ------------------------------------------------------- */
             PARM       KWD(RCDT) TYPE(*DEC) LEN(7 0) RTNVAL(*YES) +
                          PROMPT('CL var for RCDT        (7 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for completion time                         */
          /* ------------------------------------------------------- */
             PARM       KWD(RCTM) TYPE(*DEC) LEN(6 0) RTNVAL(*YES) +
                          PROMPT('CL var for RCTM        (6 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for status route code                       */
          /* ------------------------------------------------------- */
             PARM       KWD(STRC) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for STRC         (10)')

          /* ------------------------------------------------------- */
          /* Returnvalue for route entry sequence                    */
          /* ------------------------------------------------------- */
             PARM       KWD(RES) TYPE(*DEC) LEN(3 0) RTNVAL(*YES) +
                          PROMPT('CL var for RES         (3 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for support level                           */
          /* ------------------------------------------------------- */
             PARM       KWD(SUL) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
                          PROMPT('CL var for SUL           (1)')

          /* ------------------------------------------------------- */
          /* Returnvalue for status                                  */
          /* ------------------------------------------------------- */
             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
                          PROMPT('CL var for STAT          (5)')

          /* ------------------------------------------------------- */
          /* Returnvalue for status date                             */
          /* ------------------------------------------------------- */
             PARM       KWD(SRDT) TYPE(*DEC) LEN(7 0) RTNVAL(*YES) +
                          PROMPT('CL var for SRDT        (7 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for status time                             */
          /* ------------------------------------------------------- */
             PARM       KWD(SRTM) TYPE(*DEC) LEN(6 0) RTNVAL(*YES) +
                          PROMPT('CL var for SRTM        (6 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for severity 1                              */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV1) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
                          PROMPT('CL var for SEV1          (5)')

          /* ------------------------------------------------------- */
          /* Returnvalue for severity 2                              */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV2) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
                          PROMPT('CL var for SEV2          (5)')

          /* ------------------------------------------------------- */
          /* Returnvalue for severity 3                              */
          /* ------------------------------------------------------- */
             PARM       KWD(SEV3) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
                          PROMPT('CL var for SEV3          (5)')

          /* ------------------------------------------------------- */
          /* Returnvalue for priority                                */
          /* ------------------------------------------------------- */
             PARM       KWD(PRI) TYPE(*DEC) LEN(3 0) RTNVAL(*YES) +
                          PROMPT('CL var for PRI         (3 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for escalation counter                      */
          /* ------------------------------------------------------- */
             PARM       KWD(ESC) TYPE(*DEC) LEN(3 0) RTNVAL(*YES) +
                          PROMPT('CL var for ESC         (3 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for escalation date                         */
          /* ------------------------------------------------------- */
             PARM       KWD(IEDT) TYPE(*DEC) LEN(7 0) RTNVAL(*YES) +
                          PROMPT('CL var for IEDT        (7 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for escalation time                         */
          /* ------------------------------------------------------- */
             PARM       KWD(IETM) TYPE(*DEC) LEN(6 0) RTNVAL(*YES) +
                          PROMPT('CL var for IETM        (6 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for registered by                           */
          /* ------------------------------------------------------- */
             PARM       KWD(RUID) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for RUID         (10)')

          /* ------------------------------------------------------- */
          /* Returnvalue for allocated by                            */
          /* ------------------------------------------------------- */
             PARM       KWD(AUID) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for AUID         (10)')

          /* ------------------------------------------------------- */
          /* Returnvalue for incident type code                      */
          /* ------------------------------------------------------- */
             PARM       KWD(ITCP) TYPE(*CHAR) LEN(5) RTNVAL(*YES) +
                          PROMPT('CL var for ITCP          (5)')

          /* ------------------------------------------------------- */
          /* Returnvalue for configuration list number               */
          /* ------------------------------------------------------- */
             PARM       KWD(CFL) TYPE(*DEC) LEN(5 0) RTNVAL(*YES) +
                          PROMPT('CL var for CFL         (5 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for configuration item                      */
          /* ------------------------------------------------------- */
             PARM       KWD(CFIC) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for CFIC         (12)')

          /* ------------------------------------------------------- */
          /* Returnvalue for user field 1                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF1) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for USF1         (10)')

          /* ------------------------------------------------------- */
          /* Returnvalue for user field 2                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF2) TYPE(*CHAR) LEN(25) RTNVAL(*YES) +
                          PROMPT('CL var for USF2         (25)')

          /* ------------------------------------------------------- */
          /* Returnvalue for user field 3                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF3) TYPE(*DEC) LEN(15 0) RTNVAL(*YES) +
                          PROMPT('CL var for USF3       (15 0)')

          /* ------------------------------------------------------- */
          /* Returnvalue for short descriptiob                       */
          /* ------------------------------------------------------- */
             PARM       KWD(SHID) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for SHID         (50)')

