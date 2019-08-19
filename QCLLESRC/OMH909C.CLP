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
 /* Program ....: OMH909C                                            */
 /* Function ...: Check object, member or message                    */
 /* Author .....: W.S. Jongman.                                      */
 /* Startdate ..: 13-03-1991.                                        */
 /* Parameters .: &OBJL, library.                                    */
 /*               &OBJC, object code.                                */
 /*               &MBRC, object member.                              */
 /*               &OBJT, object type.                                */
 /*               &STAT, program status.                             */
 /*                                                                  */
 /* This program checks if the object specified in the parameters    */
 /* exists. If it is the status field is filled with '*NORM' else    */
 /* it is filled with '*TERM'.                                       */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&OBJL &OBJC &MBRC &OBJT &STAT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&OBJL)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJC)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&MBRC)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJT)       TYPE(*CHAR) LEN(9)
             DCL        VAR(&STAT)       TYPE(*CHAR) LEN(5)

 /* ---------------------------------------------------------------- */
 /* Declare program variables                                        */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&RCV)        TYPE(*CHAR) LEN(37)
             DCL        VAR(&OBCL)       TYPE(*CHAR) LEN(20)
             DCL        VAR(&OTYP)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&RCVL)       TYPE(*CHAR) LEN(4)
             DCL        VAR(&MID)        TYPE(*CHAR) LEN(7)

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Prefill with status '*NORM' (normal)                             */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&STAT) VALUE('*NORM')

 /* ---------------------------------------------------------------- */
 /* Member is blank, do normal object checking                       */
 /* ---------------------------------------------------------------- */
             IF         COND(&MBRC *EQ '         ') THEN(DO)

             CHGVAR     VAR(&OTYP) VALUE(&OBJT)
             CHGVAR     VAR(&RCVL) VALUE(X'00000025')
             CHGVAR     VAR(&OBCL) VALUE(&OBJC *CAT &OBJL)

             CALL       PGM(QUSROBJD) PARM(&RCV &RCVL 'OBJD0100' +
                          &OBCL &OTYP)

             MONMSG     MSGID(CPF9801 CPF9810 CPF9811 CPF9812 +
                          CPF9814) EXEC(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

             MONMSG     MSGID(CPF3C07) EXEC(DO) /* Error on command */
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             RCVMSG     MSGTYPE(*FIRST) RMV(*NO) MSGID(&MID)
             IF         COND(&MID *EQ 'CPD3C31') THEN(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO


             GOTO       CMDLBL(ERROR)
             ENDDO
             GOTO       CMDLBL(ENDPGM)

             ENDDO

 /* ---------------------------------------------------------------- */
 /* Member is filled with membername                                 */
 /* ---------------------------------------------------------------- */
             IF         COND(&OBJT *EQ '*FILE     ') THEN(DO)

             RTVMBRD    FILE(&OBJL/&OBJC) MBR(&MBRC)
             MONMSG     MSGID(CPF0000) EXEC(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             ENDDO
             GOTO       CMDLBL(ENDPGM)

             ENDDO

 /* ---------------------------------------------------------------- */
 /* Member is filled with msgid                                      */
 /* ---------------------------------------------------------------- */
             IF         COND(&OBJT *EQ '*MSGF     ') THEN(DO)

             RTVMSG     MSGID(&MBRC) MSGF(&OBJL/&OBJC)
             MONMSG     MSGID(CPF2407 CPF2419 CPF2499) EXEC(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             ENDDO
             GOTO       CMDLBL(ENDPGM)

             ENDDO

 /* ---------------------------------------------------------------- */
 /* Member is filled with something else                             */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)

 /* ---------------------------------------------------------------- */
 /* Handle unexpected errors                                         */
 /* ---------------------------------------------------------------- */
 ERROR:
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             RCVMSG     MSGTYPE(*FIRST) RMV(*NO) MSGID(&MID)
             IF         COND(&MID *EQ 'CPF9803') THEN(DO)
             CALL       PGM(OMX426) PARM(&OBJC &OBJL &OBJT &MBRC +
                          *STAT)
             ENDDO

             CALL       PGM(OMH928C)
             MONMSG     MSGID(CPF0000)
             RETURN

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 ENDPGM:
             RMVMSG     PGMQ(*SAME) CLEAR(*ALL)
             RETURN
             ENDPGM
