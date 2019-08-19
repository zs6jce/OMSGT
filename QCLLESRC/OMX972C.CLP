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
 /* Program ....: OMX971C                                            */
 /* Function ...: Change object owner                                */
 /* Author .....: P.C.C. Wortel                                      */
 /* Startdate ..: 06-09-1994.                                        */
 /* Parameters .: &OBJC, object code.                                */
 /*               &OBJT, object type.                                */
 /*               &OBJL, object library.                             */
 /*               &COWN, Current owner                               */
 /*               &STAT, Status                                      */
 /*                                                                  */
 /* This program retrieves the owner of an object                    */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&OBJC &OBJT &OBJL &COWN &STAT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&OBJC)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJT)       TYPE(*CHAR) LEN(9)
             DCL        VAR(&OBJL)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&COWN)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&STAT)       TYPE(*CHAR) LEN(5)

 /* ---------------------------------------------------------------- */
 /* Declare program variables                                        */
 /* ---------------------------------------------------------------- */

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Assume normal ending                                             */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&STAT) VALUE('*NORM')

 /* ---------------------------------------------------------------- */
 /* Retrieve the owner                                               */
 /* ---------------------------------------------------------------- */
             RTVOBJD    OBJ(&OBJL/&OBJC) OBJTYPE(&OBJT) OWNER(&COWN)

 /* ---------------------------------------------------------------- */
 /* Go to end of program                                             */
 /* ---------------------------------------------------------------- */
             GOTO       CMDLBL(ENDPGM)

 /* ---------------------------------------------------------------- */
 /* Handle unexpected errors                                         */
 /* ---------------------------------------------------------------- */
 ERROR:
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             MONMSG     MSGID(CPF0000)
             RETURN

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 ENDPGM:
             RMVMSG     CLEAR(*ALL)
             MONMSG     MSGID(CPF0000)
             RETURN
             ENDPGM
