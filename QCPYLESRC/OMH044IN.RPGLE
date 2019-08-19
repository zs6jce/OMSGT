      *
      *          ------------------------------------------
      *          D#44IN Incident data queue entry
      *
      *          ------------------------------------------
      *          General header section  (128 bytes)
      *          ------------------------------------------
      *          I44USI - This user
      *          I44JNM - Job name
      *          I44JNR - Job number
      *          I44ENT - Entity
      *          I44ACT - Action
      *          I44R1  - Reserved
      *
      *          ------------------------------------------
      *          Incident section       (869 bytes)
      *          ------------------------------------------
      *          I44HED - Helpdesk
      *          I44INC - Incident
      *          I44OSL - Old support level
      *          I44OST - Old status
      *          I44OSR - Old status route
      *          I44NSL - New support level
      *          I44NST - New status
      *          I44NSL - New support level
      *          ------------------------------------------
     D D#44IN          DS          1024
     D  I44USI                 1     10
     D  I44JNM                11     20
     D  I44JNR                21     26  0
     D  I44ENT                27     28
     D  I44ACT                29     30
     D  I44R1                 31    128
      *
     D  I44HED               129    133
     D  I44INC               134    143
     D  I44OSL               144    144
     D  I44OST               145    149
     D  I44OSR               150    159
     D  I44NSL               160    160
     D  I44NST               161    165
     D  I44NSR               166    175
