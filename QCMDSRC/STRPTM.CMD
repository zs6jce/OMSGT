 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH048)                                                         */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*INTERACT *IPGM *IREXX *EXEC *IMOD)                           */
 /*   *$$ PMTFILE(OMHDSP)                                                     */
 /*   *$$ MSGF(OMHMSG)                                                        */
 /*   *$$ HLPPNLGRP(OMSCMD)                                                   */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAIN)                                                   */
 /*   *$$\ENDOM                                                               */
             CMD        PROMPT('Predefined Text Maintenance')

             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) DFT(*CURRENT) +
                          SPCVAL((*CURRENT '     ')) +
                          CHOICE('Code, *CURRENT') +
                          PROMPT('HelpDesk')

             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')
