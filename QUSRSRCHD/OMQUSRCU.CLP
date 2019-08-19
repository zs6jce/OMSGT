 /* ---------------------------------------------------------------- */
 /*                                                                  */
 /*  Program ....: OMQUSRCU                                          */
 /*  Description : Customer exit API                                 */
 /*  Author .....: Remain Software                                   */
 /*                                                                  */
 /*  Parameters .: P#CUSC      Customer code                (I)      */
 /*                P#CCTC      Customer contact code        (I)      */
 /*                            If blanks then no contact             */
 /*                P#ACTC      Action                       (I)      */
 /*                            '20' - Change                         */
 /*                            '60' - Add                            */
 /*                P#USF1      User field 1                 (I/O)    */
 /*                P#F1ST      User field 1 return status   (O)      */
 /*                            *TERM - Field is in error             */
 /*                            *NORM - Field is ok                   */
 /*                P#USF2      User field 2                 (I/O)    */
 /*                P#F2ST      User field 2 return status   (O)      */
 /*                            *TERM - Field is in error             */
 /*                            *NORM - Field is ok                   */
 /*                P#USF3      User field 3                 (I/O)    */
 /*                P#F3ST      User field 3 return status   (O)      */
 /*                            *TERM - Field is in error             */
 /*                            *NORM - Field is ok                   */
 /*                P#STAT      Program status               (O)      */
 /*                            *TERM - Error                         */
 /*                            *NORM - Ok                            */
 /*                            *EXIT - Exit                          */
 /*                                                                  */
 /* The customer maintenance program checks if there is a program    */
 /* with this name. If there is, the program is called during the    */
 /* edit session of the maintain customer function.                  */
 /*                                                                  */
 /* You can check the individual user fields and set each field      */
 /* in error by filling the appropriate return status. Messages      */
 /* must be send to the caller of this program. The message can      */
 /* be used to indicate the cause of the error.                      */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

 /* ---------------------------------------------------------------- */
 /* Program                                                          */
 /* ---------------------------------------------------------------- */
             PGM        PARM(&CUSC &CCTC &ACTC &USF1 &F1ST &USF2 +
                          &F2ST &USF3 &F3ST &STAT)

          /* ------------------------------------------------------- */
          /* Declare parameter-variables                             */
          /* ------------------------------------------------------- */
             DCL        VAR(&CUSC) TYPE(*CHAR) LEN(010)
             DCL        VAR(&CCTC) TYPE(*CHAR) LEN(010)
             DCL        VAR(&ACTC) TYPE(*CHAR) LEN(002)
             DCL        VAR(&USF1) TYPE(*CHAR) LEN(010)
             DCL        VAR(&F1ST) TYPE(*CHAR) LEN(005)
             DCL        VAR(&USF2) TYPE(*CHAR) LEN(025)
             DCL        VAR(&F2ST) TYPE(*CHAR) LEN(005)
             DCL        VAR(&USF3) TYPE(*DEC)  LEN(015 0)
             DCL        VAR(&F3ST) TYPE(*CHAR) LEN(005)
             DCL        VAR(&STAT) TYPE(*CHAR) LEN(005)

          /* ------------------------------------------------------- */
          /* Monitor                                                 */
          /* ------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Processing starts here                                           */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* For all actions                                         */
          /* ------------------------------------------------------- */
             IF         COND((&ACTC *EQ '20') *OR (&ACTC *EQ '60')) +
                          THEN(DO)

          /* ------------------------------------------------------- */
          /* For customers                                           */
          /* ------------------------------------------------------- */
             IF         COND(&CCTC *EQ '  ') THEN(DO)

          /* ------------------------------------------------------- */
          /* Check field 1                                           */
          /* ------------------------------------------------------- */
             IF         COND(&USF1 *EQ ' ') THEN(DO)
             CHGVAR     VAR(&F1ST) VALUE('*TERM')
             SNDPGMMSG  MSG('User field 1 may not be blanks')
             ENDDO

          /* ------------------------------------------------------- */
          /* Check field 2                                           */
          /* ------------------------------------------------------- */
             IF         COND(&USF2 *NE 'XXX') THEN(DO)
             CHGVAR     VAR(&F2ST) VALUE('*TERM')
             CHGVAR     VAR(&USF2) VALUE('XXX')
             SNDPGMMSG  MSG('User field 2 must be ''XXX''. The field +
                          was changed')
             ENDDO

          /* ------------------------------------------------------- */
          /* Check field 3                                           */
          /* ------------------------------------------------------- */
             IF         COND(&USF3 *LE 0) THEN(DO)
             CHGVAR     VAR(&F3ST) VALUE('*TERM')
             SNDPGMMSG  MSG('User field 3 must be greater than zero')
             ENDDO

             ENDDO

          /* ------------------------------------------------------- */
          /* For customer contacts                                   */
          /* ------------------------------------------------------- */
             IF         COND(&CCTC *NE '  ') THEN(DO)

          /* ------------------------------------------------------- */
          /* Check field 1                                           */
          /* ------------------------------------------------------- */
             IF         COND(&USF1 *EQ ' ') THEN(DO)
             CHGVAR     VAR(&F1ST) VALUE('*TERM')
             SNDPGMMSG  MSG('Contact User field 1 may not be blanks')
             ENDDO

          /* ------------------------------------------------------- */
          /* Check field 2                                           */
          /* ------------------------------------------------------- */
             IF         COND(&USF2 *NE 'XXX') THEN(DO)
             CHGVAR     VAR(&F2ST) VALUE('*TERM')
             CHGVAR     VAR(&USF2) VALUE('XXX')
             SNDPGMMSG  MSG('User field 2 must be ''XXX''. The field +
                          was changed')
             ENDDO

          /* ------------------------------------------------------- */
          /* Check field 3                                           */
          /* ------------------------------------------------------- */
             IF         COND(&USF3 *LE 0) THEN(DO)
             CHGVAR     VAR(&F3ST) VALUE('*TERM')
             SNDPGMMSG  MSG('User field 3 must be greater than zero')
             ENDDO
             ENDDO
             ENDDO

          /* ------------------------------------------------------- */
          /* Exit                                                    */
          /* ------------------------------------------------------- */
             GOTO       CMDLBL(ENDPGM)

/* ----------------------------------------------------------------- */
/* An error occured                                                  */
/* ----------------------------------------------------------------- */
 ERROR:
             SNDPGMMSG  MSGID(OMQ5012) MSGF(OMSAPI) +
                          MSGDTA('OMQUSRCU') MSGTYPE(*ESCAPE)
             MONMSG     MSGID(CPF0000)
             RETURN

/* ----------------------------------------------------------------- */
/* End of Program                                                    */
/* ----------------------------------------------------------------- */
 ENDPGM:
             RETURN
             ENDPGM
