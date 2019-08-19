      * ----------------------------------------------------------- *
      * Description                                                 *
      * ----------------------------------------------------------- *
      *                                                             *
      * Copyright ..: Remain BV, Netherlands. All rights reserved.  *
      * Program ....: OMQPRTHS                                      *
      * Function ...: HMS - Get arerage from history file           *
      * Parms in ...: P#Hedc - Helpdesk Code                        *
      *               P#Inc# - Incident Number                      *
      *               P#Avgt - Average Time                         *
      *               P#Strc - Current route                        *
      *               P#Stac - Current status                       *
      *               P#Fstr - From status route                    *
      *               P#Fsta - From status                          *
      *               P#Tstr   To status route                      *
      *               P#Tsta   To status                            *
      *               P#Stat - Status                               *
      *                        *NORM - Normal ending                *
      *                        *NOTF - Not found                    *
      *                        *FND  - Found                        *
      *                        *NCAL - No calculation range found   *
      *                        *TERM - General error                *
      *                                                             *
      * ----------------------------------------------------------- *
     H USRPRF(*OWNER)
     H DATFMT(*ISO)
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * File specifications                                         *
      * ----------------------------------------------------------- *
     FOMHILL3   IF   E           K DISK
     FOMSREL1   IF   E           K DISK
     FOMEPRL1   IF   E           K DISK
      * ----------------------------------------------------------- *
      /EJECT
      * ----------------------------------------------------------- *
      * Program arrays
      * ----------------------------------------------------------- *
     DRouteArr         S             10    DIM(500)
     DStatArr          S              5    DIM(500)
     DHoursArr         S              7  2 DIM(500)
     DDayArr           S              1    DIM(7)
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

     D D#Dat           DS
     D  D#Dat1                 1     10D
     D  D#Dat2                11     20D

     D D#Fld           DS
     D  CC                     1      1
     D  YY                     2      3
     D  MM                     4      5
     D  DD                     6      7

     D D#Iso           DS
     D  ICC                    1      2
     D  IYY                    3      4
     D  IS1                    5      5
     D  IMM                    6      7
     D  IS2                    8      8
     D  IDD                    9     10

     D D#Tim           DS
     D  D#Tim1                 1      8T
     D  D#Tim2                 9     16T
     D  D#Optm                17     24T
     D  D#Cltm                25     32T

     D D#Rec           DS
     D  D#Inc#                 1     10
     D  D#Hedc                11     15
     D  D#Fsta                16     20
     D  D#Tsta                21     25
     D  D#Fstr                28     37
     D  D#Tstr                38     47

     DSkip             S               N
     DStaOk            S               N
      *          ------------------------------------------
      *          Named constants
      *          ------------------------------------------
     D C#TERM          C                   CONST('*TERM')
     D C#NOTF          C                   CONST('*NOTF')
     D C#NCAL          C                   CONST('*NCAL')
     D C#FND           C                   CONST('*FND')
     D C#CMP           C                   CONST('*****')
     D C#NORM          C                   CONST('*NORM')
     D C#Optm          C                   CONST('090000')
     D C#Cltm          C                   CONST('170000')
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
     C                   PARM                    P#Avgt
     C                   PARM                    P#Strc
     C                   PARM                    P#Stac
     C                   PARM                    P#Fstr
     C                   PARM                    P#Fsta
     C                   PARM                    P#Tstr
     C                   PARM                    P#Tsta
     C                   PARM                    P#Stat

      *          ------------------------------------------
      *          End program when no parms passed
      *          ------------------------------------------
     C     D#PARM        IFLT      10                                           B01
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF                                                  E01

      *          ------------------------------------------
      *          Assume normal ending
      *          ------------------------------------------
     C                   Eval      P#Stat = C#NORM
      *          ------------------------------------------
      *          Get the opening hours of the helpdesk
      *          ------------------------------------------
     C                   Exsr      SRGetHrs
      *          ------------------------------------------
      *          Get always start date and time
      *          ------------------------------------------
     C                   Exsr      SRGetAdd
      *          ------------------------------------------
      *          Fill the arrays with all the changes max 500
      *          ------------------------------------------
     C                   Exsr      SRFilArr
      *          ------------------------------------------
      *          Calculate the selection hours
      *          ------------------------------------------
     C                   Exsr      SRGetSts
      *
     C                   If        W#Hrs <= 0
     C                   Eval      P#Avgt = 0,01
     C                   Else
     C                   Eval      P#Avgt = W#Hrs
     C                   EndIf
      *
      *          ------------------------------------------
      *          End the program
      *          ------------------------------------------
     C                   SETON                                        LR
     C                   RETURN
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGetAdd = Get the time and date the incident was added      *
      * ------------------------------------------------------------ *
     C     SRGetAdd      Begsr

      *          ------------------------------------------
      *          Check if the incident exists
      *          ------------------------------------------
     C                   Eval      P#Msid = 'OMQA005'
     C     $Hill3        Chain     OMHILL3
     C                   If        %Found(OMHILL3)
      *          ------------------------------------------
      *          Get the day from the date yhe incident was added
      *          Starting with 1 = Mon, 2 = Tue etc.
      *          ------------------------------------------
     C                   Move      ILINDT        P#Date
     C                   Call      'OMX929C'     $929C
      *          ------------------------------------------
      *          Get the date and time the incident was added
      *          ------------------------------------------
     C                   Move      ILINDT        D#Fld
     C                   Exsr      SRFmtDat
     C                   Move      D#Iso         D#Dat1
     C                   Move      ILINTM        D#Tim1
     C                   EndIf

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   Endsr
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGetSts = Get all status or status route changes            *
      * ------------------------------------------------------------ *
     C     SRGetSts      Begsr
      *
     C                   Eval      P#Stat = C#NOTF
     C                   Eval      W#Hrs = *Zero
     C                   Eval      W#Tot = *Zero
      *
     C                   Select
      *          ------------------------------------------
      *          Get the *FIRST route with *FIRST status and
      *          *NONE route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr = '0' And P#Fsta = '0' And
     C                             P#Tstr = '0' And P#Tsta = '0'
     C                   Eval      X = 1
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      P#Stat = C#FND
      *          ------------------------------------------
      *          Get the *ALL route with *FIRST status and
      *          *NONE route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr = '1' And P#Fsta = '0' And
     C                             P#Tstr = '0' And P#Tsta = '0'
     C                   Eval      X = 1
     C                   Movel     RouteArr(X)   W#Rte
     C                   Movel     StatArr(X)    W#Sta
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      X = X + 1
     C     W#Rte         Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = W#Sta
     C                   Z-Add     HoursArr(X)   W#Did
     C                   Exsr      SRFmtHrs
     C                   EndIf
     C                   Eval      X = X + 1
     C     W#Rte         Lookup    RouteArr(X)                            99
     C                   Eval      P#Stat = C#FND
     C                   EndDo
      *          ------------------------------------------
      *          Get the *FIRST route with XXX status and
      *          *NONE route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr = '0' And P#Fsta <> '0' And
     C                             P#Tstr = '0' And P#Tsta =  '0'
     C                   Eval      X = 1
     C     P#Fsta        Lookup    StatArr(X)                             99
     C                   If        *IN99 = *On
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      P#Stat = C#FND
     C                   EndIf
      *          ------------------------------------------
      *          Get the XXX route with *FIRST status and
      *          *NONE route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr <> '0' And P#Fstr <> '1' And
     C                             P#Fsta =  '0' And
     C                             P#Tstr =  '0' And P#Tsta =  '0'
     C                   Eval      X = 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   If        *IN99 = *On
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      P#Stat = C#FND
     C                   EndIf
      *          ------------------------------------------
      *          Get the XXX route with XXX status and
      *          *NONE route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr <> '0' And P#Fstr <> '1' And
     C                             P#Fsta <> '0' And
     C                             P#Tstr =  '0' And P#Tsta =  '0'
     C                   Eval      X = 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = P#Fsta
     C                   Z-Add     HoursArr(X)   W#Did
     C                   Exsr      SRFmtHrs
     C                   Eval      P#Stat = C#FND
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   EndDo
      *          ------------------------------------------
      *          Get the *ALL route with XXX status and
      *          *NONE route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr = '1' And P#Fsta <> '0' And
     C                             P#Tstr = '0' And P#Tsta =  '0'
     C                   Eval      X = 1
     C     P#Fsta        Lookup    StatArr(X)                             99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = P#Fsta
     C                   Z-Add     HoursArr(X)   W#Did
     C                   Exsr      SRFmtHrs
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Fsta        Lookup    StatArr(X)                             99
     C                   Eval      P#Stat = C#FND
     C                   EndDo
      *          ------------------------------------------
      *          Get the *FIRST route with *FIRST status and
      *          the *LAST route with *COMPLETE status
      *          ------------------------------------------
     C                   When      P#Fstr = '0' And P#Fsta = '0' And
     C                             P#Tstr = '1' And P#Tsta = '1'
     C                   Eval      X = 1
     C     C#CMP         Lookup    StatArr(X)                             99
     C                   If        *IN99 = *On
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      P#Stat = C#FND
     C                   EndIf
      *          ------------------------------------------
      *          Get the *FIRST route with *FIRST status and
      *          the *LAST route with XXX status
      *          ------------------------------------------
     C                   When      P#Fstr = '0' And P#Fsta =  '0' And
     C                             P#Tstr = '1' And P#Tsta <> '0' And
     C                             P#Tsta <> '1'
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     P#Tsta        Lookup    StatArr(X)                             99
     C                   Dow       *IN99 = *On AND X < 500
     C                   Eval      X = X + 1
     C     P#Tsta        Lookup    StatArr(X)                             99
     C                   If        X > Y
     C                   Eval      Y = X
     C                   EndIf
     C                   EndDo
     C                   Eval      X = 1
      *
     C                   Do        Y
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      P#Stat = C#FND
     C                   Eval      X = X + 1
     C                   EndDo
      *          ------------------------------------------
      *          Get the *FIRST route with XXX status and
      *          the *LAST route with XXX status
      *          ------------------------------------------
     C                   When      P#Fstr = '0' And P#Fsta <> '0' And
     C                             P#Tstr = '1' And P#Tsta <> '0' And
     C                             P#Tsta <> '1'
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     P#Tsta        Lookup    StatArr(X)                             99
     C                   Dow       *IN99 = *On AND X < 500
     C                   Eval      X = X + 1
     C     P#Tsta        Lookup    StatArr(X)                             99
     C                   If        X > Y
     C                   Eval      Y = X
     C                   EndIf
     C                   EndDo
      *
     C                   Eval      X = 1
     C     P#Fsta        Lookup    StatArr(X)                             99
     C                   If        *IN99 = *On
     C                   Do        Y
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      P#Stat = C#FND
     C                   Eval      X = X + 1
     C                   If        X > Y
     C                   Leave
     C                   EndIf
     C                   EndDo
     C                   EndIf
      *          ------------------------------------------
      *          Get the *FIRST route with *FIRST status and
      *          the XXX route with XXX status
      *          ------------------------------------------
     C                   When      P#Fstr =  '0' And P#Fsta =  '0' And
     C                             P#Tstr <> '0' And P#Tstr <> '1' And
     C                             P#Tsta <> '0' And P#Tsta <> '1'
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     P#Tstr        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = P#Tsta
     C                   Eval      Y = X
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Tstr        Lookup    RouteArr(X)                            99
     C                   EndDo
      *
     C                   Eval      X = 1
     C                   Do        Y
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      X = X + 1
     C                   Eval      P#Stat = C#FND
     C                   EndDo
      *          ------------------------------------------
      *          Get the *FIRST route with XXX status and
      *          the *LAST route with *COMPLETE status
      *          ------------------------------------------
     C                   When      P#Fstr = '0' And P#Fsta <> '0' And
     C                             P#Tstr = '1' And P#Tsta =  '1'
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     C#CMP         Lookup    StatArr(X)                             99
     C                   If        *IN99 = *On
     C                   Eval      Y = 1
     C     P#Fsta        Lookup    StatArr(Y)                             99
     C                   If        *IN99 = *On
     C                   Eval      X = X - Y
     C                   Do        X
     C                   Z-Add     HoursArr(Y)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      Y = Y + 1
     C                   Eval      P#Stat = C#FND
     C                   EndDo
     C                   EndIf
     C                   EndIf
      *          ------------------------------------------
      *          Get the XXX route with *FIRST status and
      *          the *LAST route with *COMPLETE status
      *          ------------------------------------------
     C                   When      P#Fstr <> '0' And P#Fstr <> '1' And
     C                             P#Fsta =  '0' And P#Tstr =  '1' And
     C                             P#Tsta =  '1'
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     C#CMP         Lookup    StatArr(X)                             99
     C                   If        *IN99 = *On
     C                   Eval      Y = 1
     C     P#Fstr        Lookup    RouteArr(Y)                            99
     C                   If        *IN99 = *On
     C                   Eval      X = X - Y
     C                   Do        X
     C                   Z-Add     HoursArr(Y)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      Y = Y + 1
     C                   Eval      P#Stat = C#FND
     C                   EndDo
     C                   EndIf
     C                   EndIf
      *          ------------------------------------------
      *          Get the *FIRST route with XXX status and
      *          the XXX route with XXX status
      *          ------------------------------------------
     C                   When      P#Fstr =  '0' And P#Fsta <> '0' And
     C                             P#Tstr <> '0' And P#Tstr <> '1' And
     C                             P#Tsta <> '0' And P#Tsta <> '1'
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     P#Tstr        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = P#Tsta
     C                   Eval      Y = X
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Tstr        Lookup    RouteArr(X)                            99
     C                   EndDo
      *
     C                   Eval      X = 1
     C     P#Fsta        Lookup    StatArr(X)                             99
     C                   If        *IN99 = *On
     C                   Eval      Y = Y - X
     C                   Do        Y
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      X = X + 1
     C                   Eval      P#Stat = C#FND
     C                   EndDo
     C                   EndIf
      *          ------------------------------------------
      *          Get the XXX route with XXX status and
      *          the XXX route with XXX status
      *          ------------------------------------------
     C                   When      P#Fstr <> '0' And P#Fstr <> '1' And
     C                             P#Fsta <> '0' And
     C                             P#Tstr <> '0' And P#Tstr <> '1' And
     C                             P#Tsta <> '0' And P#Tsta <> '1'
     C                   Eval      StaOk = *Off
     C                   Eval      X = 1
     C                   Eval      Y = 0
     C     P#Tstr        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = P#Tsta
     C                   Eval      Y = X
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Tstr        Lookup    RouteArr(X)                            99
     C                   EndDo
      *
     C                   Eval      X = 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) = P#Fsta
     C                   Eval      StaOk = *On
     C                   Leave
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   EndDo
      *
     C                   If        StaOk
     C                   Do        Y
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   Eval      X = X + 1
     C                   Eval      P#Stat = C#FND
     C                   EndDo
     C                   EndIf
      *          ------------------------------------------
      *          Get the *FIRST route with *FIRST status and
      *          the *LAST route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr =  '0' And P#Fsta = '0' And
     C                             P#Tstr =  '1' And P#Tsta = '0'
     C                   Eval      X = 1
     C                   Eval      W#Rte = RouteArr(X)
     C     W#Rte         Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) <> C#CMP
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   EndIf
     C                   Eval      X = X + 1
     C     W#Rte         Lookup    RouteArr(X)                            99
     C                   Eval      P#Stat = C#FND
     C                   EndDo
      *          ------------------------------------------
      *          Get the XXX route with *FIRST status and
      *          the *LAST route with *NONE status
      *          ------------------------------------------
     C                   When      P#Fstr <> '0' And P#Fstr <> '1' And
     C                             P#Fsta =  '0' And
     C                             P#Tstr =  '1' And P#Tsta = '0'
     C                   Eval      X = 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   Dow       *IN99 = *On AND X < 500
     C                   If        StatArr(X) <> C#CMP
     C                   Z-Add     HoursArr(X)   W#Hrs
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   EndIf
     C                   Eval      X = X + 1
     C     P#Fstr        Lookup    RouteArr(X)                            99
     C                   Eval      P#Stat = C#FND
     C                   EndDo
      *
     C                   Other
     C                   Eval      P#Stat = C#NCAL
     C                   Endsl
      *
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   Endsr
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFmtHrs = Format the Hours                                  *
      * ------------------------------------------------------------ *
     C     SRFmtHrs      Begsr
      *
     C                   Eval      W#Hrh = *Zero
     C                   Eval      W#Min = *Zero
     C                   Eval      W#Hr1 = *Zero
     C                   Eval      W#Mi1 = *Zero
      *
     C                   If        W#Hrs <> 0
     C     W#Hrs         Div       1             W#Hrh
     C                   Mvr                     W#Min
     C                   EndIf
      *
     C                   If        W#Did <> 0
     C     W#Did         Div       1             W#Hr1
     C                   Mvr                     W#Mi1
     C                   EndIf
      *
     C                   If        (W#Min + W#Mi1) >= 0,60
     C                   Eval      W#Min = (W#Min + W#Mi1) - 0,60
     C                   Eval      W#Hrs = (W#Hrh + W#Hr1 + 1,00 +
     C                                      W#Min)
     C                   Else
     C                   Eval      W#Hrs = W#Hrh + W#Hr1 + W#Min + W#Mi1
     C                   EndIf

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFmtDat = Format the date to *ISO                           *
      * ------------------------------------------------------------ *
     C     SRFmtDat      Begsr

     C                   If        CC = '1'
     C                   Eval      ICC = '20'
     C                   Else
     C                   Eval      ICC = '19'
     C                   Endif
     C                   Eval      IYY = YY
     C                   Eval      IS1 = '-'
     C                   Eval      IMM = MM
     C                   Eval      IS2 = '-'
     C                   Eval      IDD = DD

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRCalHrs = Calculate the hours                               *
      * ------------------------------------------------------------ *
     C     SRCalHrs      Begsr
      *          ------------------------------------------
      *          Extract the number of days between the date
      *          the incident was added and the incident was
      *          changed or closed
      *          ------------------------------------------
     C     D#Dat2        Subdur    D#Dat1        W#Day:*D               99
     C                   If        *IN99 = *Off
      *
     C                   Eval      Skip = *Off
      *          ----------------------------------------------
      *          Correct the day with the start and the end day
      *          ----------------------------------------------
     C                   Eval      X = P#Day + 2
      *          ----------------------------------------------
      *          If the day = 5 (Friday) and the difference
      *          between the start date and end date = 3 (Monday)
      *          then correct the W#Day to 0 days
      *          ----------------------------------------------
     C                   If        P#Day = 5 And W#Day = 3
     C                   Eval      W#Day = 1
     C                   EndIf
      *          ----------------------------------------------
      *          Point to the right day
      *          ----------------------------------------------
     C                   If        X > 7
     C                   Eval      X = 1
     C                   EndIf
      *          ----------------------------------------------
      *          Calculate the hours for the number of days
      *          ----------------------------------------------
     C                   Do        W#Day
      *
     C                   If        DayArr(X) = '1' and Skip
      *
     C                   Eval      W#Hrh = *Zero
     C                   Eval      W#Min = *Zero
     C                   Eval      W#Hr1 = *Zero
     C                   Eval      W#Mi1 = *Zero
      *
     C                   If        W#Hrop <> 0
     C     W#Hrop        Div       1             W#Hrh
     C                   Mvr                     W#Min
     C                   EndIf
      *
     C                   If        W#Hrs <> 0
     C     W#Hrs         Div       1             W#Hr1
     C                   Mvr                     W#Mi1
     C                   EndIf
      *
     C                   If        (W#Min + W#Mi1) >= 0,60
     C                   Eval      W#Min = (W#Min + W#Mi1) - 0,60
     C                   Eval      W#Hrs = (W#Hrs + W#Hrh + 1,00 +
     C                                      W#Min) - W#Mi1
     C                   Else
     C                   Eval      W#Hrs = W#Hrs + W#Hrh + W#Min
     C                   EndIf
      *
     C                   EndIf
      *
     C                   If        X < 7
     C                   Eval      X = X + 1
     C                   Else
     C                   Eval      X = 1
     C                   EndIf
     C                   Eval      Skip = *On
      *
     C                   EndDo
      *          ------------------------------------------
      *          Calculate the minutes between the time
      *          the incident was changed and the closing time
      *          of the helpdesk or the time the incident was
      *          changed
      *          ------------------------------------------
     C                   Eval      W#Avt = *Zero
     C                   Eval      W#Min = *Zero
     C                   Eval      W#Hr1 = *Zero
     C                   Eval      W#Mi1 = *Zero
      *
     C                   If        W#Day = 0
     C                   Eval      W#Hrs = *Zero
     C     D#Tim2        Subdur    D#Tim1        W#Avt:*MN              99
     C                   Else
     C     D#Cltm        Subdur    D#Tim1        W#Avt:*MN              99
     C                   EndIf
     C                   If        *IN99 = *Off
      *          ------------------------------------------
      *          Extract the hours from the minutes
      *          ------------------------------------------
     C                   Dow       W#Avt >= 60
     C                   Eval      W#Avt = W#Avt - 60
     C                   Eval      W#Hrs = W#Hrs + 1,00
     C                   EndDo
      *          ------------------------------------------
      *          Extract the minutes from the hours
      *          ------------------------------------------
     C                   Eval      W#Min = W#Avt / 100
     C                   If        W#Hrs <> 0
     C     W#Hrs         Div       1             W#Hr1
     C                   Mvr                     W#Mi1
     C                   EndIf
      *          ------------------------------------------
      *          Add the minutes to the hours
      *          ------------------------------------------
     C                   If        (W#Min + W#Mi1) >= 0,60
     C                   Eval      W#Min = (W#Min + W#Mi1) - 0,60
     C                   Eval      W#Hrs = (W#Hrs + 1,00 +
     C                                      W#Min) - W#Mi1
     C                   Else
     C                   Eval      W#Hrs = W#Hrs + W#Min
     C                   EndIf
      *
     C                   EndIf
      *          ------------------------------------------
      *          Extract the number of minutes between the time
      *          the incident was added and the opening time
      *          of the helpdesk
      *          ------------------------------------------
     C                   Eval      W#Avt = *Zero
     C                   Eval      W#Min = *Zero
     C                   Eval      W#Hr1 = *Zero
     C                   Eval      W#Mi1 = *Zero
      *
     C                   If        W#Day > 0
     C     D#Tim2        Subdur    D#Optm        W#Avt:*MN              99
     C                   If        *IN99 = *Off
      *          ------------------------------------------
      *          Extract the hours from the minutes
      *          ------------------------------------------
     C                   Dow       W#Avt >= 60
     C                   Eval      W#Avt = W#Avt - 60
     C                   Eval      W#Hrs = W#Hrs + 1,00
     C                   EndDo
      *          ------------------------------------------
      *          Extract the minutes from the hours
      *          ------------------------------------------
     C                   Eval      W#Min = W#Avt / 100
     C                   If        W#Hrs <> 0
     C     W#Hrs         Div       1             W#Hr1
     C                   Mvr                     W#Mi1
     C                   EndIf
      *          ------------------------------------------
      *          Add the minutes to the hours
      *          ------------------------------------------
     C                   If        (W#Min + W#Mi1) >= 0,60
     C                   Eval      W#Min = (W#Min + W#Mi1) - 0,60
     C                   Eval      W#Hrs = (W#Hrs + 1,00 +
     C                                      W#Min) - W#Mi1
     C                   Else
     C                   Eval      W#Hrs = W#Hrs + W#Min
     C                   EndIf
      *
     C                   EndIf
     C                   EndIf
     C                   EndIf
      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRFilArr = Fill the array with all the changes               *
      * ------------------------------------------------------------ *
     C     SRFilArr      Begsr

      *          ----------------------------------------------
      *          Extract the status routes and status codes and
      *          calculate the time
      *          ----------------------------------------------
     C                   Clear                   RouteArr
     C                   Clear                   StatArr
     C                   Clear                   HoursArr
      *          ------------------------------------------
      *          Check if the incident is changed
      *          ------------------------------------------
     C                   Eval      Y = 1
     C                   Eval      W#Tot = *Zero
     C                   Eval      P#Msid = 'OMQA00A'
     C     $Hill3        Setll(E)  OMHILL3
     C     $Hill3        Reade(E)  OMHILL3
     C                   Dow       Not %EOF(OMHILL3)
      *
     C                   Movel     ILMSGD        D#Rec
     C                   Move      ILINDT        D#Fld
     C                   Exsr      SRFmtDat
     C                   Move      D#Iso         D#Dat2
     C                   Move      ILINTM        D#Tim2
     C                   Exsr      SRCalHrs
     C                   Movel     D#Fstr        RouteArr(Y)
     C                   Movel     D#Fsta        StatArr(Y)
     C                   Z-Add     W#Hrs         HoursArr(Y)
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
      *          --------------------------------------------------
      *          Get the day from the date yhe incident was changed
      *          Starting with 1 = Mon, 2 = Tue etc.
      *          --------------------------------------------------
     C                   Move      ILINDT        P#Date
     C                   Call      'OMX929C'     $929C
      *
     C     $Hill3        Reade(E)  OMHILL3
     C                   Z-Add     *Zero         W#Hrs
     C                   Eval      D#Dat1 = D#Dat2
     C                   Eval      D#Tim1 = D#Tim2
     C                   Eval      Y = Y + 1
     C                   EndDo
     C
     C     *Blanks       Lookup    StatArr(Y)                             99
      *          ------------------------------------------
      *          Check if the incident is COMPLETE else use
      *          the current date and time
      *          ------------------------------------------
     C                   Eval      P#Msid = 'OMQA008'
     C     $Hill3        Chain     OMHILL3
     C                   If        %Found(OMHILL3)
     C                   Move      ILINDT        D#Fld
     C                   Exsr      SRFmtDat
     C                   Move      D#Iso         D#Dat2
     C                   Move      ILINTM        D#Tim2
     C                   Exsr      SRCalHrs
     C                   If        Y > 1
     C                   Movel     D#Tstr        RouteArr(Y)
     C                   Movel     D#Tsta        StatArr(Y)
     C                   Z-Add     W#Hrs         HoursArr(Y)
     C                   Eval      Y = Y + 1
     C                   EndIf
     C                   Eval      W#Did = W#Tot
     C                   Exsr      SRFmtHrs
     C                   Eval      W#Tot = W#Hrs
     C                   If        D#Tstr <> *Blanks
     C                   Movel     D#Tstr        RouteArr(Y)
     C                   Else
     C                   Movel     P#Strc        RouteArr(Y)
     C                   EndIf
     C                   Movel     C#CMP         StatArr(Y)
     C                   Z-Add     W#Tot         HoursArr(Y)
     C                   Else
      *          ------------------------------------------------------
      *          The incident was not closed so use the current
      *          date and time execpt the current statas = *CMP
      *          ------------------------------------------------------
     C                   Movel     P#Strc        RouteArr(Y)
     C                   If        P#Stac <> '*CMP'
     C                   Move      *Date         D#Dat2
     C                   Move      D#TIME        D#Tim2
     C                   Exsr      SRCalHrs
     C                   Movel     P#Stac        StatArr(Y)
     C                   Z-Add     W#Hrs         HoursArr(Y)
     C                   Else
     C                   Movel     C#CMP         StatArr(Y)
     C                   Z-Add     0,01          HoursArr(Y)
     C                   EndIf
     C                   EndIf

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR                                                  UNDERFLW
      * ------------------------------------------------------------ *
      /EJECT
      * ------------------------------------------------------------ *
      * SRGetHrs = Get the opening hours of the helpdesk             *
      * ------------------------------------------------------------ *
     C     SRGetHrs      Begsr

      *          ----------------------------------------------
      *          Get the opening hours of the helpdesk
      *          ----------------------------------------------
     C                   Clear                   DayArr
     C     P#Hedc        Chain     OMEPRL1
     C                   If        %Found(OMEPRL1)
     C                   Movea     PRDOWA        DayArr
     C                   Move      PRSPTM        D#Optm
     C                   Move      PREPTM        D#Cltm
     C                   Move      PRSPTM        D#Tim1
     C                   Move      PREPTM        D#Tim2
     C     D#Tim2        Subdur    D#Tim1        W#Div:*MN              99
     C                   If        *IN99 = *Off
     C                   Dow       W#Div >= 60
     C                   Eval      W#Hrop = W#Hrop + 1
     C                   Eval      W#Div = W#Div - 60
     C                   Enddo
     C                   If        W#Div <> 0
     C                   Eval      W#Hrd = W#Div / 100
     C                   EndIf
     C                   Eval      W#Hrop = W#Hrop + W#Hrd
     C                   EndIf
     C                   Else
      *          ----------------------------------------------
      *          If no opening hours found then use normal working week
      *          ----------------------------------------------
     C                   Movea     '0111110'     DayArr
     C                   Move      090000        D#Optm
     C                   Move      170000        D#Cltm
     C                   Move      090000        D#Tim1
     C                   Move      170000        D#Tim2
     C                   Eval      W#Hrop = 8,00
     C                   EndIf

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

     C     *LIKE         DEFINE    RESTRC        W#Rte
     C     *LIKE         DEFINE    RESTRC        W#Tstr
     C     *LIKE         DEFINE    RESTRC        P#Fstr
     C     *LIKE         DEFINE    RESTRC        P#Strc
     C     *LIKE         DEFINE    RESTAT        P#Fsta
     C     *LIKE         DEFINE    RESTAT        P#Stac
     C     *LIKE         DEFINE    RESTAT        W#Sta
     C     *LIKE         DEFINE    RESTRC        P#Tstr
     C     *LIKE         DEFINE    RESTAT        P#Tsta

     C                   Move      *Blanks       P#Stat            5
     C                   Z-Add     *Zeros        P#Day             6 0
     C                   Move      *Blanks       P#Date            7
      *          ------------------------------------------
      *          Work field definition.
      *          ------------------------------------------
     C     *LIKE         DEFINE    ILINTM        W#Adt
     C     *LIKE         DEFINE    ILINTM        W#Stt

     C                   Move      *Blanks       RecOk             4

     C                   Z-Add     *Zeros        W#Avd             7 0
     C                   Z-Add     *Zeros        W#Avt             7 0
     C                   Z-Add     *Zeros        W#Div             7 0
     C                   Z-Add     *Zeros        W#Day             7 0
     C                   Z-Add     *Zeros        W#Hrs             7 2
     C                   Z-Add     *Zeros        W#Tot             7 2
     C                   Z-Add     *Zeros        W#Tel             7 0
     C                   Z-Add     *Zeros        P#Avgt            7 2
     C                   Z-Add     *Zeros        W#Hrop            7 2
     C                   Z-Add     *Zeros        W#Hrd             7 2
     C                   Z-Add     *Zeros        W#Daw             7 0
     C                   Z-Add     *Zeros        W#Min             7 2
     C                   Z-Add     *Zeros        W#Hrh             7 0
     C                   Z-Add     *Zeros        W#Mi1             7 2
     C                   Z-Add     *Zeros        W#Hr1             7 0
     C                   Z-Add     *Zeros        W#Did             7 2
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
      *          Parm list for OMX929C
      *          ------------------------------------------
     C     $929C         Plist
     C                   Parm                    P#Date
     C                   Parm                    P#Day
     C                   Parm                    P#Stat

      *          ------------------------------------------
      *          End of subroutine
      *          ------------------------------------------
     C                   ENDSR
