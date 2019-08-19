 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH018C)                                                        */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*INTERACT *IPGM *IREXX *EXEC *IMOD)                           */
 /*   *$$ PMTFILE(OMHDSP)                                                     */
 /*   *$$ MSGF(OMHMSG)                                                        */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
             CMD        PROMPT('Incident Maintenance')

             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) DFT(*CURRENT) +
                          SPCVAL((*BLANKS ' ') (*CURRENT ' ')) +
                          CHOICE('Code, *CURRENT') PROMPT('HelpDesk')

             PARM       KWD(CTN) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*BLANKS ' ') (*ALL ' ')) +
                          CHOICE('Call number, *ALL') +
                          PROMPT('Customer Call Number')

             PARM       KWD(CUSC) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL ' ')) CHOICE('Code, *ALL') +
                          PROMPT('Customer')

             PARM       KWD(CCTC) TYPE(*CHAR) LEN(10) DFT(*ALL) +
                          SPCVAL((*ALL ' ')) CHOICE('Code, *ALL') +
                          PROMPT('Customer Contact')

             PARM       KWD(STAT) TYPE(*CHAR) LEN(5) CONSTANT(' ')
