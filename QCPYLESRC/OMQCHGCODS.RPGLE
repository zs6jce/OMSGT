      *
      *          ------------------------------------------
      *          This source member contains the input
      *          definitions used for the OMQCHGCO api
      *          Change Customer Contact
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Interface level  (INPUT)
      *          The interface level ensures that the parameters
      *          passed to the program are recognized by the API.
      *          ------------------------------------------
     D L4INTL          DS             8
      *
      *          ------------------------------------------
      *          Action code  (INPUT)
      *          The action code describes the mode in which the
      *          API is to be activated for the object
      *          ------------------------------------------
     D L4ACTC          DS             2
      *
      *          ------------------------------------------
      *          Input buffer processing array
      *          L4EOP - End of processing  (true/false)
      *          L4UPD - Update call        (true/false)
      *          L4UPB - Update call buffer (true/false)
      *          ------------------------------------------
     D L4PRC           DS
     D  L4EOP                  1      1
     D  L4UPD                  2      2
     D  L4UPB                  3      3
     D  L4DU2                  4     40
      *
      *          ------------------------------------------
      *          Output buffer error data
      *          ------------------------------------------
     D L4ERR           DS
     D  L4MSID                 1      7
     D  L4MSTP                 8      8
     D  L4MSGD                 9    136
      *
      *          ------------------------------------------
      *          Input / Output buffer field error data
      *          ------------------------------------------
     D L4FLD           DS
     D  L4MSQ                  1     10
     D  L4FLDA                11    130
      *
      *          ------------------------------------------
      *          Input buffer data structure for all codes
      *          ------------------------------------------
     D L4BUF           DS          1024
     D  L4CUSC                 1     10
     D  L4CCTC                11     20
     D  L4CNTI                21     21
     D  L4TITL                22     51
     D  L4NAMD                52    101
     D  L4TEL#               102    116
     D  L4EMAC               117    166
     D  L4CFL#               167    169P 0
     D  L4USF1               170    179
     D  L4USF2               180    204
     D  L4USF3               205    212P 0
     D  L4USFA               205    212
     D  L4NCCT               213    222
     D  L4MTDI               223    223
