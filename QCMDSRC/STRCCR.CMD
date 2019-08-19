 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH017)                                                         */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(OMHDSP)                                                     */
 /*   *$$ MSGF(QCPFMSG)                                                       */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
             CMD        PROMPT('Customer Call Registration')

             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) DFT(*BLANKS) +
                          SPCVAL((*BLANKS ' ')) +
                          CHOICE('Code, *BLANKS') +
                          PROMPT('HelpDesk')

             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')
