 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH041C)                                                        */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(*NONE)                                                      */
 /*   *$$ MSGF(QCPFMSG)                                                       */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
             CMD        PROMPT('Customer Mail Extensions')

             PARM       KWD(CUSC) TYPE(*CHAR) LEN(10) MIN(1) +
                          CHOICE('Customer code') PROMPT('Customer +
                          code')

             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')
