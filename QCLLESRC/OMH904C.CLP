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
 /* Program ....: OMX904C                                            */
 /* Function ...: Get message data from message file                 */
 /* Author .....: W.S. Jongman.                                      */
 /* Startdate ..: 17-01-1991.                                        */
 /* Parameters .: &MSGID, message identifier                         */
 /*               &MSGF, message file to retrieve from               */
 /*               &MSGFLIB, message file library                     */
 /*               &MSGTEXT, 132 long message text.                   */
 /*                                                                  */
 /* This program is called from program's which send messages that   */
 /* require message data to be sent with it. This program retrieves  */
 /* that message data from the specified message file.               */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&MSGID &MSGF &MSGFLIB &MSGTEXT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&MSGID)   TYPE(*CHAR) LEN(7)
             DCL        VAR(&MSGF)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&MSGFLIB) TYPE(*CHAR) LEN(10)
             DCL        VAR(&MSGTEXT) TYPE(*CHAR) LEN(132)

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Retrieve message from message queue                              */
 /* ---------------------------------------------------------------- */
             RTVMSG     MSGID(&MSGID) MSGF(&MSGFLIB/&MSGF) +
                          MSG(&MSGTEXT)
             GOTO       CMDLBL(ENDPGM)

 /* ---------------------------------------------------------------- */
 /* Handle unexpected messages                                       */
 /* ---------------------------------------------------------------- */
 ERROR:

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 ENDPGM:
             RETURN
             ENDPGM
