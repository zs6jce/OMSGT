      *
      *          ------------------------------------------
      *          This source member contains the input
      *          definitions used for the User Interface with
      *          Request (OMQUSIRE) API.
      *          ------------------------------------------
      *
      *          ------------------------------------------
      *          Interface level  (INPUT)
      *          The interface level ensures that the parameters
      *          passed to the program are recognized by the API.
      *          ------------------------------------------
     D R2INTL          DS             8
      *
      *
      *          ------------------------------------------
      *          Action code  (INPUT)
      *          The action code describes the mode in which the
      *          user interface API is to be activated for the
      *          current request.
      *          ------------------------------------------
     D R2ACTC          DS             2
      *
      *          ------------------------------------------
      *          Processing array  (INPUT)
      *          The processing array.
      *
      *          R2EOP  - End of processing    0=No, 1=Yes
      *          R2RDSP - Redisplay            0=No, 1=Yes
      *          R2FILL - Prefill              0=No, 1=Yes, 2=Field
      *          R2PAPC - Protect application  0=No, 1=Yes
      *          ------------------------------------------
     D R2PRCA          DS
     D  R2EOP                  1      1
     D  R2RDSP                 2      2
     D  R2FILL                 3      3
     D  R2PAPC                 4      4
      *
      *          ------------------------------------------
      *          Standard error buffer (OUTPUT)
      *          Standard error buffer. This buffer contains the
      *          message id in case of an error. The message must
      *          exist in OMSAPI.
      *          R2MSID - Message id
      *          R2MSTP - Message type
      *          R2MSGD - Message data
      *          ------------------------------------------
     D R2ERR           DS
     D  R2MSID                 1      7
     D  R2MSTP                 8      8
     D  R2MSGD                 9    136
      *
      *          ------------------------------------------
      *          Number of function keys (INPUT)
      *          This field contains the number of function keys
      *          that are passed to the program. The actual function
      *          keys exist in array FIN.
      *          ------------------------------------------
     D R2FDS#          DS
     D  R2FUN#                 1      2B 0
      *
      *          ------------------------------------------
      *          Function key used (OUTPUT)
      *          ------------------------------------------
     D R2KDS#          DS
     D  R2FKU#                 1      2B 0
      *
      *          ------------------------------------------
      *          Cursor field (INPUT/OUTPUT)
      *          For prefill cursor (R2FILL=2), this field contains
      *          the field that must be pre-filled. When F4 is pressed
      *          on a non promptable field, then the field is passed
      *          back in this parameter.
      *          ------------------------------------------
     D R2CFLC          DS            10
      *
      *
      *          ------------------------------------------
      *          Key buffer
      *          This parameter contains the key to the entity.
      *          ------------------------------------------
     D R2KEY           DS
     D  R2KAPP                 1      5
     D  R2KERR                 6     15
      *
      *          ------------------------------------------
      *          Request file buffer
      *          ------------------------------------------
     D R2REQ           DS
     D  R2APPC                 1      5
     D  R2ERR#                 6     15
     D  R2ERRT                16     20
     D  R2PDLC                21     25
     D  R2REAC                26     30
     D  R2PRIC                31     35
     D  R2USID                36     45
     D  R2ADDR                46     53
     D  R2USRI                54     61
     D  R2TEL#                62     76
     D  R2WCDT                77     83  0
     D  R2RCDT                84     90  0
     D  R2INDT                91     97  0
     D  R2INTM                98    103  0
     D  R2SHED               104    153
     D  R2FREE               154    185
