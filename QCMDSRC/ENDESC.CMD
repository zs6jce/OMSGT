 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH026C)                                                        */
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
 /* Command ....: ENDESC                                             */
 /* Function ...: End escalation                                     */
 /* Author .....: Remain Software                                    */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('End Escalation Job if Active')

             PARM       KWD(HEDC) TYPE(*NAME) LEN(8) DFT(*ALL) +
                          SPCVAL((*ALL)) MIN(0) PROMPT(MQP0002 2) +
                          /* Help Desk */

