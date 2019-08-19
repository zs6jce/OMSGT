      *
      *          ------------------------------------------
      *          This source member contains the input
      *          definitions used for the OMQPRTIN api
      *          Print incident
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Interface level  (INPUT)
      *          The interface level ensures that the parameters
      *          passed to the program are recognized by the API.
      *          ------------------------------------------
     D I3INTL          DS             8
      *
      *          ------------------------------------------
      *          Action code  (INPUT)
      *          The action code describes the mode in which the
      *          API is to be activated for the object
      *          ------------------------------------------
     D I3ACTC          DS             2
      *
      *          ------------------------------------------
      *          Input buffer processing array
      *          I3EOP - END OF PROCESS (Close all files)  0/1
      *          I3PIR - Print incident relations          0/1
      *          I3PRR - Print request relations           0/1
      *          I3PFI - Print fix  information            0/1
      *          I3PSI - Print solution information        0/1
      *          I3LOD - List or detail (1 = Detail)       0/1
      *          I3PIP - Print incident per page           0/1
      *          I3PCP - Print cover page                  0/1
      *          I3PEP - Print end page                    0/1
      *          I3QAH - Print question and answer history 0/1
      *          I3PIT - Print incident test all/s/c/s/o   0/1/2/3/4
      *          I3CLP - Close after print                 0/1
      *          I3AVE - Print avarage                     0/1
      *          ------------------------------------------
     D I3PRC           DS
     D  I3EOP                  1      1
     D  I3PIR                  2      2
     D  I3PRR                  3      3
     D  I3PFI                  4      4
     D  I3PSI                  5      5
     D  I3LOD                  6      6
     D  I3PIP                  7      7
     D  I3PCP                  8      8
     D  I3PEP                  9      9
     D  I3QAH                 10     10
     D  I3PIT                 11     11
     D  I3CLP                 12     12
     D  I3AVE                 13     13
     D  I3HST                 14     14
     D  I3DU2                 15     40
      *
      *          ------------------------------------------
      *          Output buffer error data
      *          ------------------------------------------
     D I3ERR           DS
     D  I3MSID                 1      7
     D  I3MSTP                 8      8
     D  I3MSGD                 9    136
      *
      *          ------------------------------------------
      *          Input / Output buffer field error data
      *          ------------------------------------------
     D I3FLD           DS
     D  I3MSQ                  1     10
     D  I3FLDA                11    130
      *
      *          ------------------------------------------
      *          Input buffer data structure for all codes
      *          ------------------------------------------
     D I3BUF           DS
     D  I3HEDC                 1      5
     D  I3INC                  6     15
     D  I3CTN                 16     25
     D  I3IDTF                26     32  0
     D  I3IDTT                33     39  0
     D  I3ITMF                40     45  0
     D  I3ITMT                46     51  0
     D  I3CDTF                52     58  0
     D  I3CDTT                59     65  0
     D  I3STRC                66     75
     D  I3SULC                76     76
     D  I3SEV1                77     81
     D  I3SEV2                82     86
     D  I3SEV3                87     91
     D  I3PRIF                92     93P 0
     D  I3PRIT                94     95P 0
     D  I3RUID                96    105
     D  I3AUID               106    115
     D  I3ITPC               116    120
     D  I3USF1               121    130
     D  I3USF2               131    155
     D  I3USF3               156    163P 0
     D  I3ESCD               164    166
     D  I3ESC#               167    168P 0
     D  I3ESDT               169    175  0
     D  I3ALSC               176    185
     D  I3STAT               186    190
     D  I3CUSC               191    200
     D  I3CCTC               201    210
     D  I3FSTR               211    220
     D  I3FSTA               221    225
     D  I3TSTR               226    235
     D  I3TSTA               236    240
