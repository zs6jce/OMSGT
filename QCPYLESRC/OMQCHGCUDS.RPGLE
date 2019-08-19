      *
      *          ------------------------------------------
      *          This source member contains the input
      *          definitions used for the OMQCHGCU api
      *          Change Customer
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Interface level  (INPUT)
      *          The interface level ensures that the parameters
      *          passed to the program are recognized by the API.
      *          ------------------------------------------
     D K4INTL          DS             8
      *
      *          ------------------------------------------
      *          Action code  (INPUT)
      *          The action code describes the mode in which the
      *          API is to be activated for the object
      *          ------------------------------------------
     D K4ACTC          DS             2
      *
      *          ------------------------------------------
      *          Input buffer processing array
      *          K4EOP - End of processing  (true/false)
      *          K4UPD - Update call        (true/false)
      *          K4UPB - Update call buffer (true/false)
      *          ------------------------------------------
     D K4PRC           DS
     D  K4EOP                  1      1
     D  K4UPD                  2      2
     D  K4UPB                  3      3
     D  K4DU2                  4     40
      *
      *          ------------------------------------------
      *          Output buffer error data
      *          ------------------------------------------
     D K4ERR           DS
     D  K4MSID                 1      7
     D  K4MSTP                 8      8
     D  K4MSGD                 9    136
      *
      *          ------------------------------------------
      *          Input / Output buffer field error data
      *          ------------------------------------------
     D K4FLD           DS
     D  K4MSQ                  1     10
     D  K4FLDA                11    130
      *
      *          ------------------------------------------
      *          Input buffer data structure for all codes
      *          ------------------------------------------
     D K4BUF           DS          1024
     D  K4CUSC                 1     10
     D  K4NAMD                11     60
     D  K4ADST                61    110
     D  K4ZIPC               111    135
     D  K4ADRE               136    185
     D  K4COUD               186    235
     D  K4USF1               236    245
     D  K4USF2               246    270
     D  K4USF3               271    278P 0
     D  K4USFA               271    278
     D  K4NCUS               279    288
