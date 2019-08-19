 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMQRTVCUCP)                                                     */
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
 /* Command ....: OMQRTVCU                                           */
 /* Function ...: Retrieve Customer API                              */
 /* Author .....: Remain Software                                    */
 /* Module .....: Helpdesk                                           */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Retrieve Customer')

          /* ------------------------------------------------------- */
          /* Customer                                                */
          /* ------------------------------------------------------- */
             PARM       KWD(CUSC) TYPE(*CHAR) LEN(10) MIN(1) +
                          PROMPT('Customer')

          /* ------------------------------------------------------- */
          /* Returnval for customer name                             */
          /* ------------------------------------------------------- */
             PARM       KWD(NAME) TYPE(*CHAR) LEN(50) RTNVAL(*YES) +
                          PROMPT('CL var for NAME         (50)')

          /* ------------------------------------------------------- */
          /* Returnval for customer street                           */
          /* ------------------------------------------------------- */
             PARM       KWD(STREET) TYPE(*CHAR) LEN(50) RTNVAL(*YES) +
                          PROMPT('CL var for STREET       (50)')

          /* ------------------------------------------------------- */
          /* Returnval for Customer zip                              */
          /* ------------------------------------------------------- */
             PARM       KWD(ZIP) TYPE(*CHAR) LEN(25) RTNVAL(*YES) +
                          PROMPT('CL var for ZIP          (25)')

          /* ------------------------------------------------------- */
          /* Returnval for Customer residence                        */
          /* ------------------------------------------------------- */
             PARM       KWD(RESIDENCE) TYPE(*CHAR) LEN(50) +
                          RTNVAL(*YES) PROMPT('CL var for +
                          RESIDENCE    (50)')

          /* ------------------------------------------------------- */
          /* Returnval for Customer country                          */
          /* ------------------------------------------------------- */
             PARM       KWD(COUNTRY) TYPE(*CHAR) LEN(50) +
                          RTNVAL(*YES) PROMPT('CL var for +
                          COUNTRY      (50)')

          /* ------------------------------------------------------- */
          /* Returnval for User field 1                              */
          /* ------------------------------------------------------- */
             PARM       KWD(USF1) TYPE(*CHAR) LEN(10) RTNVAL(*YES) +
                          PROMPT('CL var for USF1         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for User field 2                              */
          /* ------------------------------------------------------- */
             PARM       KWD(USF2) TYPE(*CHAR) LEN(25) RTNVAL(*YES) +
                          PROMPT('CL var for USF2         (10)')

          /* ------------------------------------------------------- */
          /* Returnval for User field 3                              */
          /* ------------------------------------------------------- */
             PARM       KWD(USF3) TYPE(*DEC) LEN(15 0) RTNVAL(*YES) +
                          PROMPT('CL var for USF3       (15 0)')
