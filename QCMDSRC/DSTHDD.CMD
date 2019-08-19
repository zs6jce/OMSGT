 /*   *$$\CRTOM CRTCMD                                                        */
 /*   *$$ PGM(OMH039C)                                                        */
 /*   *$$ MODE(*ALL)                                                          */
 /*   *$$ ALLOW(*ALL)                                                         */
 /*   *$$ PMTFILE(OMHDSP)                                                     */
 /*   *$$ MSGF(OMHMSG)                                                        */
 /*   *$$ HLPPNLGRP(OMSCMDHD)                                                 */
 /*   *$$ HLPID(*CMD)                                                         */
 /*   *$$ HLPSCHIDX(REMAINHD)                                                 */
 /*   *$$\ENDOM                                                               */
 /* ---------------------------------------------------------------- */
 /* Description                                                      */
 /* ---------------------------------------------------------------- */
 /*                                                                  */
 /* Command ....: DSTHDD                                             */
 /* Function ...: Distribute helpdesk definitions                    */
 /* Author .....: Remain Software                                    */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             CMD        PROMPT('Distribute Definitions')

             PARM       KWD(HEDC) TYPE(*CHAR) LEN(5) SPCVAL((*ALL +
                          *ALL)) MIN(1) PROMPT('Helpdesk')

             PARM       KWD(HOPT) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*DEF) SPCVAL((*DEF '0') (*INC '1') +
                          (*ALL '2') (*NONE '9')) PROMPT('Helpdesk +
                          option')

             PARM       KWD(CUSC) TYPE(*CHAR) LEN(10) DFT(*NONE) +
                          SPCVAL((*ALL *ALL) (*NONE *NONE)) +
                          PROMPT('Customer')

             PARM       KWD(COPT) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*DEF) SPCVAL((*DEF '0') (*INC '1') +
                          (*ALL '2') (*NONE '9')) PROMPT('Customer +
                          option')

             PARM       KWD(EXTI) TYPE(*CHAR) LEN(1) RSTD(*YES) +
                          DFT(*NOTES) SPCVAL((*NOTES '1')) +
                          PROMPT('External interface')

             PARM       KWD(JOBD) TYPE(NAME1) PROMPT('Job description')

             PARM       KWD(RANGE) TYPE(LIST1) PMTCTL(COND1) +
                          PROMPT('Range of incidents')

 NAME1:      QUAL       TYPE(*NAME) DFT(*USRPRF) SPCVAL((*USRPRF +
                          *USRPRF) (*NONE *NONE))
             QUAL       TYPE(*NAME) SPCVAL((*LIBL) (*CURLIB)) +
                          PROMPT('Library')

 LIST1:      ELEM       TYPE(*CHAR) LEN(10) DFT(*FIRST) +
                          SPCVAL((*FIRST) (*ALL)) CHOICE('Number, +
                          *FIRST, *ALL') PROMPT('Lower value')

             ELEM       TYPE(*CHAR) LEN(10) DFT(*LAST) +
                          SPCVAL((*LAST) (*ONLY)) CHOICE('Number, +
                          *LAST, *ONLY') PROMPT('Upper value')

 COND1:      PMTCTL     CTL(HOPT) COND((*EQ '1'))
             PMTCTL     CTL(HOPT) COND((*EQ '2')) LGLREL(*OR)
