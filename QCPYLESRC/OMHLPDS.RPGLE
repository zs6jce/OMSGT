      *
      *          ------------------------------------------
      *          D#D7HD User portion
      *          D7USID - This user
      *          D7USCL - His class
      *          D7GRID - His first group
      *          D7GRCL - The class of his first group
      *          D7USDC - The users description
      *          D7HEDC - The ACTIVE helpdesk code
      *          D7SUL# - The support level for this helpdesk
      *          D7DUM1 - Dummy
      *          ------------------------------------------
     D D#D7HD          DS           200
     D  D7USID                 1     10
     D  D7USCL                11     20
     D  D7GRID                21     30
     D  D7GRCL                31     40
     D  D7USDC                41     90
     D  D7HEDC                91     95
     D  D7SUL#                96     96
     D  D7DUM1                97    200
     D L#D7HD          C                   CONST(200)
      *
      *          ------------------------------------------
      *          D#D8HD Helpdesk portion
      *          D8HEDC - Helpdesk code
      *          D8HEDD - Descriprion
      *          D8HDMC - Helpdesk manager
      *          D8CTFP - Call track program
      *          D8CTFL - Call track library
      *          D8INFP - Incident program
      *          D8INFL - Incident library
      *          D8DUM1 - Dummy
      *          ------------------------------------------
     D D#D8HD          DS           200
     D  D8HEDC                 1      5
     D  D8HEDD                 6     55
     D  D8HDMC                56     65
     D  D8CTFP                66     75
     D  D8CTFL                76     85
     D  D8INFP                86     95
     D  D8INFL                96    105
     D  D8DUM1               106    200
     D L#D8HD          C                   CONST(200)
