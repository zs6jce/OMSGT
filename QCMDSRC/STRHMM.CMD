 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH040C)                                                        */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(*NONE)                                                      */
 /*   *$$ MSGF(QCPFMSG)                                                       */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
             CMD        PROMPT('HelpDesk Mail Extensions')

             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) DFT(*CURRENT) +
                          SPCVAL((*CURRENT '     ')) CHOICE('Code, +
                          *CURRENT') PROMPT('HelpDesk')

             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')
