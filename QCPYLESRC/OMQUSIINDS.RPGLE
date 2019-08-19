      *
      *          ------------------------------------------
      *          This source member contains the input
      *          definitions used for the User Interface with
      *          Incident (OMQUSIIN) API.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Interface level  (INPUT)
      *          The interface level ensures that the parameters
      *          passed to the program are recognized by the API.
      *          ------------------------------------------
     D I2INTL          DS             8
      *
      *          ------------------------------------------
      *          Action code  (INPUT)
      *          The action code describes the mode in which the
      *          user interface API is to be activated for the
      *          current request.
      *          ------------------------------------------
     D I2ACTC          DS             2
      *
      *          ------------------------------------------
      *          Processing array  (INPUT)
      *          The processing array.
      *
      *          I2EOP  - End of processing    0=No, 1=Yes
      *          I2RDSP - Redisplay            0=No, 1=Yes
      *          I2FILL - Prefill              0=No, 1=Yes, 2=Field
      *          I2PHPD - Protect helpdesk     0=No, 1=Yes
      *          I2ATXT - Add text             0=No, 1=Yes
      *          ------------------------------------------
     D I2PRCA          DS
     D  I2EOP                  1      1
     D  I2RDSP                 2      2
     D  I2FILL                 3      3
     D  I2PHPD                 4      4
     D  I2ATXT                 5      5
      *
      *          ------------------------------------------
      *          Standard error buffer (OUTPUT)
      *          Standard error buffer. This buffer contains the
      *          message id in case of an error. The message must
      *          exist in OMSAPI.
      *          I2MSID - Message id
      *          I2MSTP - Message type
      *          I2MSGD - Message data
      *          ------------------------------------------
     D I2ERR           DS
     D  I2MSID                 1      7
     D  I2MSTP                 8      8
     D  I2MSGD                 9    136
      *
      *          ------------------------------------------
      *          Number of function keys (INPUT)
      *          This field contains the number of function keys
      *          that are passed to the program. The actual function
      *          keys exist in array FIN.
      *          ------------------------------------------
     D I2FDS#          DS
     D  I2FUN#                 1      2B 0
      *
      *          ------------------------------------------
      *          Function key used (OUTPUT)
      *          ------------------------------------------
     D I2KDS#          DS
     D  I2FKU#                 1      2B 0
      *
      *          ------------------------------------------
      *          Cursor field (INPUT/OUTPUT)
      *          For prefill cursor (I2FILL=2), this field contains
      *          the field that must be pre-filled. When F4 is pressed
      *          on a non promptable field, then the field is passed
      *          back in this parameter.
      *          ------------------------------------------
     D I2CFLC          DS            10
      *
      *          ------------------------------------------
      *          Key buffer
      *          This parameter contains the key to the entity.
      *          ------------------------------------------
     D I2KEY           DS
     D  I2KHED                 1      5
     D  I2KINC                 6     15
      *
      *          ------------------------------------------
      *          Input buffer data structure for all codes
      *          ------------------------------------------
     D I2INC           DS
     D  I2HEDC                 1      5
     D  I2INC#                 6     15
     D  I2CTN#                16     25
     D  I2AIN#                26     35
     D  I2INDT                36     42  0
     D  I2INTM                43     48  0
     D  I2RCDT                49     55  0
     D  I2RCTM                56     61  0
     D  I2STRC                62     71
     D  I2RES#                72     73P 0
     D  I2SUL#                74     74
     D  I2STAT                75     79
     D  I2SRDT                80     86  0
     D  I2SRTM                87     92  0
     D  I2SEV1                93     97
     D  I2SEV2                98    102
     D  I2SEV3               103    107
     D  I2PRI#               108    109P 0
     D  I2ESC#               110    111P 0
     D  I2IEDT               112    118  0
     D  I2IETM               119    124  0
     D  I2RUID               125    134
     D  I2AUID               135    144
     D  I2ITPC               145    149
     D  I2CFL#               150    152P 0
     D  I2CFIC               153    164
     D  I2USF1               165    174
     D  I2USF2               175    199
     D  I2USF3               200    207P 0
     D  I2SHID               208    257
     D  I2DUM1               258    300
