 /* ---------------------------------------------------------------- */
 /*                                                                  */
 /*  Program ....: OMQESCP                                           */
 /*  Description : Escallation Exit                                  */
 /*  Author .....: Remain Software                                   */
 /*                                                                  */
 /*  Parameters .:  &HEDC      Helpdesk           5         (I)      */
 /*                 &INC       Incident number    10        (I)      */
 /*                 &OLDPRI    Old priority       3.0       (I)      */
 /*                 &NEWPRI    New priority       3.0       (I)      */
 /*                 &STAT      Status             5         (O)      */
 /*                                                                  */
 /* This is the escallation trigger program.                         */
 /*                                                                  */
 /* This is an example program. Use this as a basis to create your   */
 /* own.                                                             */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

 /* ---------------------------------------------------------------- */
 /* Program                                                          */
 /* ---------------------------------------------------------------- */
             PGM        PARM(&HEDC &INC &OLDPRI &NEWPRI &STAT)

          /* ------------------------------------------------------- */
          /* Declare parameter-variables                             */
          /* ------------------------------------------------------- */
             DCL        VAR(&HEDC) TYPE(*CHAR) LEN(005)
             DCL        VAR(&INC) TYPE(*CHAR) LEN(010)
             DCL        VAR(&OLDPRI) TYPE(*DEC) LEN(3 0)
             DCL        VAR(&NEWPRI) TYPE(*DEC) LEN(3 0)
             DCL        VAR(&STAT) TYPE(*CHAR) LEN(005)


          /* ------------------------------------------------------- */
          /* Monitor                                                 */
          /* ------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

 /* ---------------------------------------------------------------- */
 /* Processing starts here                                           */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Exit                                                    */
          /* ------------------------------------------------------- */
             GOTO       CMDLBL(ENDPGM)

/* ----------------------------------------------------------------- */
/* An error occured                                                  */
/* ----------------------------------------------------------------- */
 ERROR:
             SNDPGMMSG  MSGID(OMQ5012) MSGF(OMSAPI) +
                          MSGDTA('OMQESCP') MSGTYPE(*ESCAPE)
             MONMSG     MSGID(CPF0000)
             RETURN

/* ----------------------------------------------------------------- */
/* End of Program                                                    */
/* ----------------------------------------------------------------- */
 ENDPGM:
             RETURN
             ENDPGM
