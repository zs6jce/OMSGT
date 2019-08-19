 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQRTVHDCP)                                                     */
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
 /* Command ....: OMQRTVHD                                           */
 /* Function ...: Retrieve Helpdesk API                              */
 /* Author .....: Remain Software                                    */
 /* Module .....: Helpdesk                                           */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Retrieve HelpDesk')

          /* ------------------------------------------------------- */
          /* Help Desk                                               */
          /* ------------------------------------------------------- */
             PARM       KWD(DESK) TYPE(*CHAR) LEN(5) MIN(1) +
                          PROMPT('HelpDesk')

          /* ------------------------------------------------------- */
          /* Returnval for description                               */
          /* ------------------------------------------------------- */
             PARM       KWD(DESC) TYPE(*CHAR) LEN(50) RTNVAL(*YES) +
                          PROMPT('CL var for DESC         (50)')

          /* ------------------------------------------------------- */
          /* Returnval for Helpdesk manager                          */
          /* ------------------------------------------------------- */
             PARM       KWD(HMGR) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for HMGR         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for Lasst customer call number                */
          /* ------------------------------------------------------- */
             PARM       KWD(LCCN) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for LCCN         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for customer call number increment            */
          /* ------------------------------------------------------- */
             PARM       KWD(CCNI)  TYPE(*DEC)  LEN(2 0) RTNVAL(*YES) +
                          PROMPT('CL var for CCNI        (2 0)')

          /* ------------------------------------------------------- */
          /* Returnval for last incident number                      */
          /* ------------------------------------------------------- */
             PARM       KWD(LINN) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for LINN         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for customer call number increment            */
          /* ------------------------------------------------------- */
             PARM       KWD(INCI)  TYPE(*DEC)  LEN(2 0) RTNVAL(*YES) +
                          PROMPT('CL var for INCI        (2 0)')

          /* ------------------------------------------------------- */
          /* Returnval for incident spool file library               */
          /* ------------------------------------------------------- */
             PARM       KWD(ISFL) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for ISFL         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for customer call exit program                */
          /* ------------------------------------------------------- */
             PARM       KWD(CCEP) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for CCEP         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for customer call exit library                */
          /* ------------------------------------------------------- */
             PARM       KWD(CCEL) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for CCEL         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for incident call exit program                */
          /* ------------------------------------------------------- */
             PARM       KWD(INEP) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for INEP         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for incident call exit library                */
          /* ------------------------------------------------------- */
             PARM       KWD(INEL) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for INEL         (10)')

