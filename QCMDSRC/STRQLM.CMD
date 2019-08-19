 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH003)                                                         */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(OMHDSP)                                                     */
 /*   *$$ MSGF(QCPFMSG)                                                       */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
             CMD        PROMPT('Question List Maintenance')

             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) DFT(*BLANKS) +
                          SPCVAL((*BLANKS ' ')) CHOICE('Code, +
                          *BLANKS') PROMPT('HelpDesk')

             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')
