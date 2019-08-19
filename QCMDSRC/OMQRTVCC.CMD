 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQRTVCCCP)                                                     */
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
 /* Command ....: OMQRTVCC                                           */
 /* Function ...: Retrieve Customer Call API                         */
 /* Author .....: Remain Software                                    */
 /* Module .....: Helpdesk                                           */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Retrieve Customer Call')

          /* ------------------------------------------------------- */
          /* Help Desk                                               */
          /* ------------------------------------------------------- */
             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) MIN(1) +
                          PROMPT('HelpDesk')

          /* ------------------------------------------------------- */
          /* Incident                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(CTN) TYPE(*CHAR) LEN(10) MIN(1) +
                          PROMPT('Call track number')

          /* ------------------------------------------------------- */
          /* Returnval for customer code                             */
          /* ------------------------------------------------------- */
             PARM       KWD(CUSC) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for CUSC         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for Customer contact code                     */
          /* ------------------------------------------------------- */
             PARM       KWD(CCTC) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for CCTC         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for income date                               */
          /* ------------------------------------------------------- */
             PARM       KWD(INDT) TYPE(*DEC) LEN(7 0) RTNVAL(*YES) +
                          PROMPT('CL var for INDT        (7 0)')

          /* ------------------------------------------------------- */
          /* Returnval for income time                               */
          /* ------------------------------------------------------- */
             PARM       KWD(INTM)  TYPE(*DEC)  LEN(6 0) RTNVAL(*YES) +
                          PROMPT('CL var for INTM        (6 0)')

          /* ------------------------------------------------------- */
          /* Returnval for completion time                           */
          /* ------------------------------------------------------- */
             PARM       KWD(COTM) TYPE(*DEC) LEN(6 0) RTNVAL(*YES) +
                          PROMPT('CL var for COTM        (6 0)')

          /* ------------------------------------------------------- */
          /* Returnval for user ID                                   */
          /* ------------------------------------------------------- */
             PARM       KWD(USID) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for USID         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for call source code                          */
          /* ------------------------------------------------------- */
             PARM       KWD(CSRC) TYPE(*CHAR) LEN(1) RTNVAL(*YES) +
                          PROMPT('CL var for CSRC          (1)')

          /* ------------------------------------------------------- */
          /* Returnval for user defined 1                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF1) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for USF1         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for user defined 2                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF2) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for USF2         (25)')

          /* ------------------------------------------------------- */
          /* Returnval for user defined 3                            */
          /* ------------------------------------------------------- */
             PARM       KWD(USF3) TYPE(*DEC) LEN(15 0) RTNVAL(*YES) +
                          PROMPT('CL var for USF3       (15 0)')
