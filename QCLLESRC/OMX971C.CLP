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
 /*               &NEWO, New owner                                   */
 /*               &CURA, Current authority                           */
 /*                      blank = command default                     */
 /*                      1     = *SAME                               */
 /*                      2     = *REVOKE                             */
 /*               &STAT, Status                                      */
 /*                                                                  */
 /* This program changes the owner of an object to the one specified.*/
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&OBJC &OBJT &OBJL &NEWO &CURA &STAT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&OBJC)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJT)       TYPE(*CHAR) LEN(9)
             DCL        VAR(&OBJL)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&NEWO)       TYPE(*CHAR) LEN(10)
             DCL        VAR(&CURA)       TYPE(*CHAR) LEN(1)
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
 /* Change the owner                                                 */
 /* ---------------------------------------------------------------- */
             IF         COND(&CURA *EQ '1') THEN(CHGOBJOWN +
                          OBJ(&OBJL/&OBJC) OBJTYPE(&OBJT) +
                          NEWOWN(&NEWO) CUROWNAUT(*SAME))
             ELSE
             IF         COND(&CURA *EQ '2') THEN(CHGOBJOWN +
                          OBJ(&OBJL/&OBJC) OBJTYPE(&OBJT) +
                          NEWOWN(&NEWO) CUROWNAUT(*REVOKE))
             ELSE
             CHGOBJOWN  OBJ(&OBJL/&OBJC) OBJTYPE(&OBJT) NEWOWN(&NEWO)

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
