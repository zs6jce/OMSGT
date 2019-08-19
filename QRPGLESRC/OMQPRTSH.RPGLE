      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Copyright ..: Remain BV, Netherlands. All Rights Reserved.  *
      *                                                             *
      * Program ....: OMQPRTHS                                      *
      * Function ...: HMS - Search history file                     *
      * Author .....: Remain Software                               *
      * Parms in ...: P#Hedc - Helpdesk Code                        *
      *               P#Inc# - Incident Number                      *
      *               P#Strc - Current route                        *
      *               P#Stac - Current status                       *
      *               P#Stat - Status                               *
      *                        *NORM - Normal ending                *
      *                        *NOTF - Not found                    *
      *                        *FND  - Found                        *
      *                        *TERM - General error                *
      *                                                             *
      * ----------------------------------------------------------- *
     H COPYRIGHT('© Remain B.V. 2002, Arthur Middelveld')
     H USRPRF(*OWNER)
     H DATFMT(*ISO)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHILL3   IF   E           K DISK
     FOMSREL1   IF   E           K DISK
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Program arrays
      * ----------------------------------------------------------- *
     DRouteArr         S             10    DIM(500)
     DStatArr          S              5    DIM(500)
      * ----------------------------------------------------------- *
      * Program data structure
      * ----------------------------------------------------------- *
     D PGM            SDS
     D  PGMQ             *PROC
     D  D#PARM           *PARMS
     D  D#PGM                  1     10
     D  D#USID               254    263
     D  D#JOB#               264    269  0
     D  D#TIME               282    287  0

     D D#Rec           DS
     D  D#Inc#                 1     10
     D  D#Hedc                11     15
     D  D#Fsta                16     20
     D  D#Tsta                21     25
     D  D#Fstr                28     37
     D  D#Tstr                38     47

      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NOTF          C                   CONST('*NOTF')
     D C#FND           C                   CONST('*FND')
     D C#CMP           C                   CONST('*****')
     D C#ALL           C                   CONST('*ALL')
     D C#NORM          C                   CONST('*NORM')
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Main line                                                   *
      * ----------------------------------------------------------- *

      *          ------------------------------------------
      *          The parameters
      *          ------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    P#Hedc
     C                   PARM                    P#Inc#
     C                   PARM                    P#Strc
     C                   PARM                    P#Stac
     C                   PARM                    P#Stat

      *          ------------------------------------------
      *          End program when no parms passed
      *          ------------------------------------------
     C     D#PARM        IFLT      5                                            B01
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF                                                  E01

      *          ------------------------------------------
      *          Assume normal ending
      *          ------------------------------------------
     C                   Eval      P#Stat = C#NORM
      *          ------------------------------------------
      *          Fill the arrays with all the changes max 500
      *          ------------------------------------------
     C                   Exsr      SRFilArr
      *
     C                   Exsr      SRChkArr
      *
      *          ------------------------------------------
      *          End the program
      *          ------------------------------------------
     C                   SETON                                        LR
     C                   RETURN
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRChkArr = Check if incident had route and/or status         *
      * ------------------------------------------------------------ *
     C     SRChkArr      Begsr

      *          ------------------------------------------
      *          Check if the incident exists
      *          ------------------------------------------
     C                   Eval      P#Stat = C#NOTF
     C                   If        P#Strc <> C#ALL
     C                   Eval      X = 1
     C     P#Strc        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On
     C                   If        P#Stac <> C#ALL
     C                   If        StatArr(X) = P#Stac
     C                   Eval      P#Stat = C#FND
     C                   Leave
     C                   EndIf
     C                   Else
     C                   Eval      P#Stat = C#FND
     C                   Leave
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Strc        Lookup    RouteArr(X)                            99
     C                   EndDo
     C                   Else
     C                   If        P#Stac <> C#ALL
     C                   Eval      X = 1
     C     P#Stac        Lookup    StatArr(X)                             99
     C                   Dow       *IN99 = *On
     C                   If        StatArr(X) = P#Stac
     C                   Eval      P#Stat = C#FND
     C                   Leave
     C                   EndIf
     C                   Eval      X = 1
     C     P#Stac        Lookup    StatArr(X)                             99
     C                   Enddo
     C                   Else
     C                   Eval      P#Stat = C#FND
     C                   EndIf
      *
     C                   EndIf
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   Endsr
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFilArr = Fill the array with all the changes               *
      * ------------------------------------------------------------ *
     C     SRFilArr      Begsr

      *          ----------------------------------------------
      *          Extract the status routes and status codes
      *          ----------------------------------------------
     C                   Clear                   RouteArr
     C                   Clear                   StatArr
      *          ------------------------------------------
      *          Check if the incident is changed
      *          ------------------------------------------
     C                   Eval      Y = 1
     C                   Eval      P#Msid = 'OMQA00A'
     C     $Hill3        Setll(E)  OMHILL3
     C     $Hill3        Reade(E)  OMHILL3
     C                   Dow       Not %EOF(OMHILL3)
      *
     C                   Movel     ILMSGD        D#Rec
     C                   Movel     D#Fstr        RouteArr(Y)
     C                   Movel     D#Fsta        StatArr(Y)
      *
     C     $Hill3        Reade(E)  OMHILL3
     C                   Eval      Y = Y + 1
     C                   EndDo
     C
     C                   Eval      Y = 1
     C     *Blanks       Lookup    StatArr(Y)                             99
      *          ------------------------------------------
      *          Check if the incident is COMPLETE
      *          ------------------------------------------
     C                   Eval      P#Msid = 'OMQA008'
     C     $Hill3        Chain     OMHILL3
     C                   If        %Found(OMHILL3)
     C                   If        Y > 1
     C                   Movel     D#Tstr        RouteArr(Y)
     C                   Movel     D#Tsta        StatArr(Y)
     C                   Eval      Y = Y + 1
     C                   EndIf
     C                   If        D#Tstr <> *Blanks
     C                   Movel     D#Tstr        RouteArr(Y)
     C                   Else
     C                   Movel     P#Strc        RouteArr(Y)
     C                   EndIf
     C                   Movel     C#CMP         StatArr(Y)
     C                   EndIf
     C*                  Else
      *          ------------------------------------------------------
      *          The incident was not closed so use the current
      *          status
      *          ------------------------------------------------------
     C*                  Movel     P#Strc        RouteArr(Y)
     C*                  If        P#Stac <> '*CMP'
     C*                  Movel     P#Stac        StatArr(Y)
     C*                  Else
     C*                  Movel     C#CMP         StatArr(Y)
     C*                  EndIf
     C*                  EndIf

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * *INZSR - Initialization program                             *
      *                                                             *
      * ----------------------------------------------------------- *
     C     *INZSR        BEGSR
      *
      *          ------------------------------------------
      *          Indicator definition
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Declare the parameters.
      *          ------------------------------------------
     C     *LIKE         DEFINE    ILHEDC        P#Hedc
     C     *LIKE         DEFINE    ILINC#        P#Inc#
     C     *LIKE         DEFINE    ILMSID        P#Msid

     C     *LIKE         DEFINE    RESTRC        P#Fstr
     C     *LIKE         DEFINE    RESTRC        P#Strc
     C     *LIKE         DEFINE    RESTAT        P#Fsta
     C     *LIKE         DEFINE    RESTAT        P#Stac
     C     *LIKE         DEFINE    RESTRC        P#Tstr
     C     *LIKE         DEFINE    RESTAT        P#Tsta

     C                   Move      *Blanks       P#Stat            5
     C                   Z-Add     *Zeros        P#Day             6 0
     C                   Move      *Blanks       P#Date            7
      *          ------------------------------------------
      *          Work field definition.
      *          ------------------------------------------
     C                   Move      *Blanks       RecOk             4

     C                   Z-Add     *Zeros        X                 5 0
     C                   Z-Add     *Zeros        Y                 5 0
      *          ------------------------------------------
      *          Key to the history file
      *          ------------------------------------------
     C     $Hill3        klist
     C                   kfld                    P#Hedc
     C                   kfld                    P#Inc#
     C                   kfld                    P#Msid

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
