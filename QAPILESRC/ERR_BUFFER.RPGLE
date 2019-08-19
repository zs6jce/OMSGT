      *
      *          ------------------------------------------
      *          Error buffer for API call.
      *
      *          The number of bytes provided is an input parameter
      *          indicating the length of the error buffer in the
      *          calling program. This field is set to 272 thus
      *          providing a 256 byte message data field.
      *
      *          D#NBP# (B4)   Number of bytes provided
      *          D#NBA# (B4)   Number of bytes available
      *          D#MSID (A7)   Message id
      *                 (A1)   Reserved
      *          D#MSGD (A250) Message data
      *          ------------------------------------------
     D P#ERR           DS
     D  D#NBP#                 1      4B 0 INZ(272)
     D  D#NBA#                 5      8B 0
     D  D#MSID                 9     15
     D  D#MSGD                17    272
