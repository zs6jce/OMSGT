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
 /* Program ....: OMH956C                                            */
 /* Function ...: Fix currently running handler check job            */
 /* Author .....: W.S. Jongman                                       */
 /* Startdate ..: 05-04-1993.                                        */
 /* Parameters .: &JOB#, Job number                                  */
 /*               &JOBN, Job name                                    */
 /*               &USID, User                                        */
 /*               &STAT, Status                                      */
 /*                                                                  */
 /* This program checks if a job is still active or on the job queue */
 /*                                                                  */
 /* ---------------------------------------------------------------- */

 /* ---------------------------------------------------------------- */
 /* Initialisation                                                   */
 /* ---------------------------------------------------------------- */
             PGM        PARM(&JOB# &JOBN &USID &STAT)

          /* ------------------------------------------------------- */
          /* Declare parameter/variables                             */
          /* ------------------------------------------------------- */
             DCL        VAR(&JOB#)    TYPE(*CHAR) LEN(6)
             DCL        VAR(&JOBN)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&USID)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&STAT)    TYPE(*CHAR) LEN(5)

          /* ------------------------------------------------------- */
          /* Declare program variables                               */
          /* ------------------------------------------------------- */
             DCL        VAR(&JOBQ)    TYPE(*CHAR) LEN(26)
             DCL        VAR(&BUF)     TYPE(*CHAR) LEN(86)
             DCL        VAR(&LOB#)    TYPE(*CHAR) LEN(4) +
                                      VALUE(X'00000056')
             DCL        VAR(&NBA#)    TYPE(*CHAR) LEN(4) +
                                      VALUE(X'00000000')
             DCL        VAR(&NUL#)    TYPE(*CHAR) LEN(4) +
                                      VALUE(X'00000000')
             DCL        VAR(&JOBI)    TYPE(*CHAR) LEN(16)
             DCL        VAR(&JOBS)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&ERR)     TYPE(*CHAR) LEN(272) +
                                      VALUE(X'00000110')

             DCL        VAR(&MSGID)   TYPE(*CHAR) LEN(7)
             DCL        VAR(&MSGDTA)  TYPE(*CHAR) LEN(256)

             DCL        VAR(&CJOB)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&CUSR)    TYPE(*CHAR) LEN(10)
             DCL        VAR(&CNBR)    TYPE(*CHAR) LEN(6)

          /* ------------------------------------------------------- */
          /* Install error monitor                                   */
          /* ------------------------------------------------------- */
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ERROR))

             CHGVAR     VAR(&STAT) VALUE('*NORM')

 /* ---------------------------------------------------------------- */
 /* Processing.                                                      */
 /* ---------------------------------------------------------------- */

          /* ------------------------------------------------------- */
          /* Compile API parameters                                  */
          /* ------------------------------------------------------- */
             CHGVAR     VAR(&JOBQ) VALUE(&JOBN *CAT &USID *CAT &JOB#)

          /* ------------------------------------------------------- */
          /* Call her                                                */
          /* ------------------------------------------------------- */
 CALLHER:
             CHGVAR     VAR(&ERR) VALUE(X'00000101')
             CALL       PGM(QUSRJOBI) PARM(&BUF &LOB# 'JOBI0100' +
                          &JOBQ &JOBI &ERR)

          /* ------------------------------------------------------- */
          /* Check if the call completed normally                    */
          /* ------------------------------------------------------- */
             CHGVAR     VAR(&NBA#) VALUE(%SST(&ERR 5 4))
             CHGVAR     VAR(%BIN(&NBA#)) VALUE(%BIN(&NBA#))

             IF         COND(&NBA# *NE &NUL#) THEN(DO)

                     /* -------------------------------------------- */
                     /* No iT dit not, check the error               */
                     /* -------------------------------------------- */
             CHGVAR     VAR(&MSGID) VALUE(%SST(&ERR 9 7))

                     /* -------------------------------------------- */
                     /* Job not found or does not exist, exit.       */
                     /* -------------------------------------------- */
             IF         COND((&MSGID *EQ 'CPF3C53')   +
                         *OR (&MSGID *EQ 'CPF3C55')   +
                         *OR (&MSGID *EQ 'CPF3C58')) THEN(DO)

             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

                     /* -------------------------------------------- */
                     /* Job currently not available, assume it is    */
                     /* still running.                               */
                     /* -------------------------------------------- */
             IF         COND(&MSGID *EQ 'CPF3C54') THEN(DO)
             CHGVAR     VAR(&STAT) VALUE('*NORM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

                     /* -------------------------------------------- */
                     /* Another error occured, log it and assume     */
                     /* the job has ended.                           */
                     /* -------------------------------------------- */
             CHGVAR     VAR(&MSGDTA) VALUE(%SST(&ERR 17 256))
             SNDPGMMSG  MSGID(&MSGID) MSGF(QCPFMSG) MSGDTA(&MSGDTA) +
                          TOPGMQ(*SAME)
             CALL       PGM(OMH928C)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

          /* ------------------------------------------------------- */
          /* The job was found, check if it is active.               */
          /* This information is retrieved from the job status       */
          /* which is at position 51(10) of the JOBI0100 format.     */
          /* ------------------------------------------------------- */
             CHGVAR     VAR(&JOBS) VALUE(%SST(&BUF 51 10))
  IF         COND((&JOBS *EQ '*ACTIVE') *OR (&JOBS *EQ +
                          '*JOBQ')) THEN(DO)

                     /* ------------------------------------------- */
                     /* It is active, if it is this job then        */
                     /* the wait is over.                           */
                     /* ------------------------------------------- */
             RTVJOBA    JOB(&CJOB) USER(&CUSR) NBR(&CNBR)

     IF      COND((&JOB# *EQ &CNBR) *AND (&USID *EQ +
                          &CUSR) *AND (&JOBN *EQ &CJOB)) THEN(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO ENDPGM
             ENDDO

     ELSE    CMD(DO)
             CHGVAR     VAR(&STAT) VALUE('*NORM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO
             ENDDO

          /* ------------------------------------------------------- */
          /* It is not active                                        */
          /* ------------------------------------------------------- */
  ELSE       CMD(DO)
             CHGVAR     VAR(&STAT) VALUE('*TERM')
             GOTO       CMDLBL(ENDPGM)
             ENDDO

 /* ---------------------------------------------------------------- */
 /* Exit                                                             */
 /* ---------------------------------------------------------------- */
 ENDPGM:
             RMVMSG     CLEAR(*ALL)
             RETURN

 /* ---------------------------------------------------------------- */
 /* Handle unexpected messages                                       */
 /* ---------------------------------------------------------------- */
 ERROR:
             CALL       PGM(OMH928C)
             MONMSG     MSGID(CPF0000)

             SNDPGMMSG  MSGID(UNX0001) MSGF(OMSMSG) MSGDTA(OMH957C) +
                          TOPGMQ(*SAME) MSGTYPE(*DIAG)
             MONMSG     MSGID(CPF0000)

             CALL       PGM(OMH928C)
             MONMSG     MSGID(CPF0000)

             RETURN
             ENDPGM
