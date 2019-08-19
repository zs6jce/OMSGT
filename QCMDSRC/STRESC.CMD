 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH0262)                                                        */
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
 /* Command ....: STRESC                                             */
 /* Function ...: Start escalation                                   */
 /* Author .....: Remain Software                                    */
 /*                                                                  */
 /* ---------------------------------------------------------------- */
             CMD        PROMPT('Start escalation')

             PARM       KWD(HEDC) TYPE(*NAME) LEN(8) DFT(*ALL) +
                          SPCVAL((*ALL) (*CURRENT)) MIN(0) +
                          PROMPT(MQP0002 2) /* Help Desk */

