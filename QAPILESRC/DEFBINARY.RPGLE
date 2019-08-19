      *
      *          ------------------------------------------
      *          Binary number definition  (alphabetic list)
      *
      *          P#APS#   Application scope
      *          P#COL#   Column
      *          P#FNR#   Function requested
      *          P#ILV#   Interface level
      *          P#ISZ#   Initial size
      *          P#KEY#   Key
      *          P#LOB#   Length of buffer
      *          P#MDL#   Message data or impromptu text length
      *          P#MXS#   Maximum size
      *          P#NOR#   Number of records
      *          P#PSC#   Program stack counter
      *          P#RSZ#   Record size
      *          P#RCC#   Record count
      *          P#ROW#   Row
      *          P#STP#   Starting position
      *          P#WTM#   Wait time
      *          P#NMQ#   Number of message queues
      *          P#LEN#   Length of entry
      *          P#LKY#   Length of key
      *          ------------------------------------------
     D                 DS
     D  P#LOB#                 1      4B 0 INZ(0)
     D  P#APS#                 5      8B 0 INZ(0)
     D  P#ILV#                 9     12B 0 INZ(0)
     D  P#PSC#                13     16B 0 INZ(0)
     D  P#WTM#                17     20B 0 INZ(0)
     D  P#FNR#                21     24B 0 INZ(0)
     D  P#ISZ#                25     28B 0 INZ(0)
     D  P#MDL#                29     32B 0 INZ(0)
     D  P#NOR#                33     36B 0 INZ(0)
     D  P#RSZ#                37     40B 0 INZ(0)
     D  P#RCC#                41     44B 0 INZ(0)
     D  P#ROW#                45     48B 0 INZ(0)
     D  P#COL#                49     52B 0 INZ(0)
     D  P#MXS#                53     56B 0 INZ(0)
     D  P#STP#                57     60B 0 INZ(0)
     D  P#KEY#                61     64B 0 INZ(0)
     D  P#NMQ#                65     68B 0 INZ(0)
     D  P#LEN#                69     72B 0 INZ(0)
     D  P#LKY#                73     76B 0 INZ(0)
