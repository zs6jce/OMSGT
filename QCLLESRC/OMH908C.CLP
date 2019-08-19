 /*   *$$\CRTOM CRTCLPGM    */
 /*   *$$ ALWRTVSRC(*NO)    */
 /*   *$$ TGTRLS(V5R2M0)    */
 /*   *$$ USRPRF(*OWNER)    */
 /*   *$$ REPLACE(*YES)     */
 /*   *$$\ENDOM             */
 /* ---------------------------------------------------------------- */
 /* Description                                                      */
 /* ---------------------------------------------------------------- */
 /*                                                                  */
 /* Program ....: OMX908C                                            */
 /* Function ...: Convert date from format to format                 */
 /* Author .....: W.S. Jongman.                                      */
 /* Startdate ..: 15-02-1991.                                        */
 /* Parameters .: &OPTI, option.                                     */
 /*               &INDT, input date.                                 */
 /*               &OTDT, output date.                                */
 /*               &INFM, input format.                               */
 /*               &OTFM, output format.                              */
 /*               &STAT, program status.                             */
 /*                                                                  */
 /* This program converts date from one format to another. If one of */
 /* the parameters contains a question mark, then the system date    */
 /* format is retrieved. See RPG OMX908 for further details.         */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&OPTI &INDT &OTDT &INFM &OTFM &STAT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&OPTI)       TYPE(*CHAR) LEN(1)
             DCL        VAR(&INDT)       TYPE(*DEC)  LEN(7 0)
             DCL        VAR(&OTDT)       TYPE(*DEC)  LEN(7 0)
             DCL        VAR(&INFM)       TYPE(*CHAR) LEN(1)
             DCL        VAR(&OTFM)       TYPE(*CHAR) LEN(1)
             DCL        VAR(&STAT)       TYPE(*CHAR) LEN(5)

 /* ---------------------------------------------------------------- */
 /* Declare program variables                                        */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&INDTC)      TYPE(*CHAR) LEN(7)
             DCL        VAR(&OTDTC)      TYPE(*CHAR) LEN(7)
             DCL        VAR(&INDTS)      TYPE(*DEC)  LEN(6 0)
             DCL        VAR(&OTDTS)      TYPE(*DEC)  LEN(6 0)
             DCL        VAR(&INDTCS)     TYPE(*CHAR) LEN(6)
             DCL        VAR(&OTDTCS)     TYPE(*CHAR) LEN(6)
             DCL        VAR(&HELP1)      TYPE(*CHAR) LEN(7)
             DCL        VAR(&HELP2)      TYPE(*CHAR) LEN(7)

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Set status to normal ending                                      */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&STAT) VALUE('*NORM')

 /* ---------------------------------------------------------------- */
 /* Quit when date is zero or all 9                                  */
 /* ---------------------------------------------------------------- */
             IF         COND((&INDT *EQ 0) *OR (&INDT *EQ 999999)) +
                          THEN(DO)
             CHGVAR     VAR(&OTDT) VALUE(&INDT)
             GOTO       CMDLBL(ENDPGM)
             ENDDO

 /* ---------------------------------------------------------------- */
 /* Fill the sorter date fields                                      */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&HELP1) VALUE(&INDT)
             CHGVAR     VAR(&HELP2) VALUE(&OTDT)
             CHGVAR     VAR(&INDTS) VALUE(%SST(&HELP1 2 6))
             CHGVAR     VAR(&OTDTS) VALUE(%SST(&HELP2 2 6))

 /* ---------------------------------------------------------------- */
 /* Fill the character date fields                                   */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&INDTC)  VALUE(&INDT)
             CHGVAR     VAR(&OTDTC)  VALUE(&OTDT)
             CHGVAR     VAR(&INDTCS) VALUE(&INDTS)
             CHGVAR     VAR(&OTDTCS) VALUE(&OTDTS)

 /* ---------------------------------------------------------------- */
 /* Check if input format must be changed to system format           */
 /* ---------------------------------------------------------------- */
             IF         COND(&INFM *EQ '?') THEN(DO)
             CVTDAT     DATE(&INDTCS) TOVAR(&OTDTCS) TOFMT(*YMD) +
                          TOSEP(*NONE)
             CHGVAR     VAR(&OTDTS) VALUE(&OTDTCS)
             CHGVAR     VAR(&OTDT) VALUE(&OTDTS)
             GOTO       CMDLBL(ENDPGM)
             ENDDO

             IF         COND(&INFM *EQ 'C') THEN(DO)
             CVTDAT     DATE(&INDTCS) TOVAR(&OTDTC) TOFMT(*CYMD) +
                          TOSEP(*NONE)
             CHGVAR     VAR(&OTDT) VALUE(&OTDTC)
             GOTO       CMDLBL(ENDPGM)
             ENDDO

 /* ---------------------------------------------------------------- */
 /* Check if output format must be changed to system format          */
 /* ---------------------------------------------------------------- */
             IF         COND(&OTFM *EQ '?') THEN(DO)
             CVTDAT     DATE(&INDTCS) TOVAR(&OTDTCS) FROMFMT(*YMD) +
                          TOSEP(*NONE)
             CHGVAR     VAR(&OTDTS) VALUE(&OTDTCS)
             CHGVAR     VAR(&OTDT) VALUE(&OTDTS)
             GOTO       CMDLBL(ENDPGM)
             ENDDO

             IF         COND(&OTFM *EQ 'C') THEN(DO)
             CVTDAT     DATE(&INDTC) TOVAR(&OTDTCS) FROMFMT(*CYMD) +
                          TOSEP(*NONE)
             CHGVAR     VAR(&OTDTS) VALUE(&OTDTCS)
             CHGVAR     VAR(&OTDT) VALUE(&OTDTS)
             GOTO       CMDLBL(ENDPGM)
             ENDDO

             CHGVAR     VAR(&OTDT) VALUE(&INDT)
             GOTO       CMDLBL(ENDPGM)
 /* ---------------------------------------------------------------- */
 /* Handle unexpected messages                                       */
 /* ---------------------------------------------------------------- */
 ERROR:
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             RMVMSG     PGMQ(*SAME *) CLEAR(*ALL)
             MONMSG     MSGID(CPF0000)
             RETURN

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 ENDPGM:     RETURN
             ENDPGM
