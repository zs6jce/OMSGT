      *
      *          ------------------------------------------
      *          This source member contains the input
      *          definitions used for the OMQCHGIN api
      *          Change Incident
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Interface level  (INPUT)
      *          The interface level ensures that the parameters
      *          passed to the program are recognized by the API.
      *          ------------------------------------------
     D I4INTL          DS             8
      *
      *          ------------------------------------------
      *          Action code  (INPUT)
      *          The action code describes the mode in which the
      *          API is to be activated for the object
      *          ------------------------------------------
     D I4ACTC          DS             2
      *
      *          ------------------------------------------
      *          Input buffer processing array
      *          I4EOP - End of processing (true/false)
      *          I4UPD - Update incident (true/false)
      *          I4UPB - Update incident buffer (true/false)
      *          I4PCM - Priority calculation method
      *                  '1'=Add
      *                  '2'=Subtract
      *                  '3'=Take as absolute value
      *          I4CCM - Priority counter calculation method
      *                  '1'=Add
      *                  '2'=Subtract
      *                  '3'=Take as absolute value
      *          I4RCM - Request connection method
      *                  '0'=Connect
      *                  '1'=Disconnect
      *          I4PDI - Propagation delay indicator.
      *                  '0'=No
      *                  '1'=Yes
      *          I4SMI - Send message indicator
      *                  '0'=No
      *                  '1'=Yes
      *          ------------------------------------------
     D I4PRC           DS
     D  I4EOP                  1      1
     D  I4UPD                  2      2
     D  I4UPB                  3      3
     D  I4PCM                  4      4
     D  I4CCM                  5      5
     D  I4RCM                  6      6
     D  I4PDI                  7      7
     D  I4SMI                  8      8
     D  I4DU2                  9     40
      *
      *          ------------------------------------------
      *          Output buffer error data
      *          ------------------------------------------
     D I4ERR           DS
     D  I4MSID                 1      7
     D  I4MSTP                 8      8
     D  I4MSGD                 9    136
      *
      *          ------------------------------------------
      *          Input / Output buffer field error data
      *          ------------------------------------------
     D I4FLD           DS
     D  I4MSQ                  1     10
     D  I4FLDA                11    130
      *
      *          ------------------------------------------
      *          Input buffer data structure for all codes
      *          ------------------------------------------
     D I4BUF           DS
     D  I4HEDC                 1      5
     D  I4INC#                 6     15
     D  I4CTN#                16     25
     D  I4AIN#                26     35
     D  I4INDT                36     42  0
     D  I4INTM                43     48  0
     D  I4RCDT                49     55  0
     D  I4RCTM                56     61  0
     D  I4STRC                62     71
     D  I4RES#                72     73P 0
     D  I4SUL#                74     74
     D  I4STAT                75     79
     D  I4SRDT                80     86  0
     D  I4SRTM                87     92  0
     D  I4SEV1                93     97
     D  I4SEV2                98    102
     D  I4SEV3               103    107
     D  I4PRI#               108    109P 0
     D  I4ESC#               110    111P 0
     D  I4IEDT               112    118  0
     D  I4IETM               119    124  0
     D  I4RUID               125    134
     D  I4AUID               135    144
     D  I4ITPC               145    149
     D  I4CFL#               150    152P 0
     D  I4CFIC               153    164
     D  I4USF1               165    174
     D  I4USF2               175    199
     D  I4USF3               200    207P 0
     D  I4SHID               208    257
     D  I4THED               258    262
     D  I4TINC               263    272
     D  I4TAPP               273    277
     D  I4TREQ               278    287
     D  I4DUM1               288    300
