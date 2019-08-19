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
 /* Program ....: OMX905C                                            */
 /* Function ...: Get selected user profile information              */
 /* Author .....: W.S. Jongman.                                      */
 /* Startdate ..: 17-01-1991.                                        */
 /* Parameters .: &USID, user i.d. or user class                     */
 /*               &USDC, description                                 */
 /*               &STAT, status                                      */
 /*                                                                  */
 /* This program is called from programs wich need user profile      */
 /* information. The field USID is filled with the user profile or   */
 /* the user class to investigate. First the program checks if       */
 /* this is an user class. If it is, then the userclass code is      */
 /* filled with the user class text.                                 */
 /* If it is not a user class, then the program checks if this user  */
 /* profile exists. If it does, then the status field is filled with */
 /* *NORM and the userprofile text is filled. Else the status field  */
 /* is filled with *????.                                            */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

             PGM        PARM(&USID &USCL &GRID &GRCL &USDC &STAT)

 /* ---------------------------------------------------------------- */
 /* Declare parameter/variables                                      */
 /* ---------------------------------------------------------------- */
             DCL        VAR(&USID)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&USCL)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&GRID)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&GRCL)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&USDC)    TYPE(*CHAR) LEN(50)
             DCL        VAR(&STAT)    TYPE(*CHAR) LEN(5)

 /* ---------------------------------------------------------------- */
 /* Install global error monitor                                     */
 /* ---------------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Check if this is a user class                                    */
 /* ---------------------------------------------------------------- */
             IF         COND(&USID *EQ '*USER     ' *OR +
                             &USID *EQ '*SYSOPR   ' *OR +
                             &USID *EQ '*PGMR     ' *OR +
                             &USID *EQ '*SECADM   ' *OR +
                             &USID *EQ '*SECOFR   ') THEN(DO)
             CHGVAR     VAR(&USDC) VALUE('User class ' *CAT &USID)
             CHGVAR     VAR(&STAT) VALUE('*NORM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

 /* ---------------------------------------------------------------- */
 /* Not a user class, retrieve profile                               */
 /* ---------------------------------------------------------------- */
             CHGVAR     VAR(&STAT) VALUE('*NORM')
             RTVUSRPRF  USRPRF(&USID) GRPPRF(&GRID) TEXT(&USDC) +
                          USRCLS(&USCL)
             MONMSG     MSGID(CPF0000) EXEC(DO)
             CHGVAR     VAR(&STAT) VALUE('*????')
             GOTO       CMDLBL(ENDPGM)
             ENDDO
             IF         COND((&GRID *NE '*NONE') *AND (&GRID *NE +
                          '          ')) THEN(RTVUSRPRF +
                          USRPRF(&GRID) USRCLS(&GRCL))

 /* ---------------------------------------------------------------- */
 /* Handle unexpected messages                                       */
 /* ---------------------------------------------------------------- */
 ERROR:

 /* ---------------------------------------------------------------- */
 /* Exit program processing.                                         */
 /* ---------------------------------------------------------------- */
 ENDPGM:     RETURN
             ENDPGM
