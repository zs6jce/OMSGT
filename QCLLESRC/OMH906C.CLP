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
 /* Program ....: OMX906C                                            */
 /* Function ...: Check object & get object description              */
 /* Author .....: W.S. Jongman.                                      */
 /* Startdate ..: 30-01-1991.                                        */
 /* Parameters .: &OBJC, object code                                 */
 /*               &OBJL, object library                              */
 /*               &OBJT, object type                                 */
 /*               &OBJD, object description                          */
 /*               &STAT, status                                      */
 /*                                                                  */
 /* This program is called from programs which need to check an      */
 /* object, or need to retrieve the object description.              */
 /* The CHKOBJ command is performed. If the command does not work    */
 /* right, then the status field is filled with the value of '*TERM' */
 /* and the object description field is filled with blanks.          */
 /* Else the status field is filled with '*NORM', and the object     */
 /* description in retrieved (if possible).                          */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&OBJC &OBJL &OBJT &OBJD &STAT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&OBJC)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJL)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&OBJT)    TYPE(*CHAR) LEN(9)
             DCL        VAR(&OBJD)    TYPE(*CHAR) LEN(50)
             DCL        VAR(&STAT)    TYPE(*CHAR) LEN(5)

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Assume normal ending                                             */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&STAT) VALUE('*NORM')

 /* ---------------------------------------------------------------- */
 /* Check the object                                                 */
 /* ---------------------------------------------------------------- */
             CHKOBJ     OBJ(&OBJL/&OBJC) OBJTYPE(&OBJT)
             MONMSG     MSGID(CPF9801 CPF9810) EXEC(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

 /* ---------------------------------------------------------------- */
 /* Object ok, retrieve the description.                             */
 /* ---------------------------------------------------------------- */
             RTVOBJD    OBJ(&OBJL/&OBJC) OBJTYPE(&OBJT) TEXT(&OBJD)
             GOTO       CMDLBL(ENDPGM)

 /* ---------------------------------------------------------------- */
 /* Handle unexpected messages                                       */
 /* ---------------------------------------------------------------- */
 ERROR:
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             RETURN

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 ENDPGM:
             RMVMSG     PGMQ(*SAME *) CLEAR(*ALL)
             RETURN
             ENDPGM
