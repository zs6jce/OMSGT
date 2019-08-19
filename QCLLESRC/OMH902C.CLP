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
 /* Program ....: OMX902C                                            */
 /* Function ...: Clear message queue of calling program.            */
 /* Author .....: W.S. Jongman.                                      */
 /* Startdate ..: 15-01-1991.                                        */
 /* Parameters .: &PGMQ, program message queue.                      */
 /*                                                                  */
 /* This is the subprogram which is called from program's with       */
 /* display interaction. It clears the message queue of the program  */
 /* which called this program.                                       */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&PGMQ)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&PGMQ) TYPE(*CHAR) LEN(10)

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Clear program message queue                                      */
 /* ---------------------------------------------------------------- */
             RMVMSG     PGMQ(*SAME &PGMQ) CLEAR(*ALL)
             GOTO       CMDLBL(END)

 /* ---------------------------------------------------------------- */
 /* Handle unexpected messages                                       */
 /* ---------------------------------------------------------------- */
 ERROR:

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 END:        RETURN
             ENDPGM
