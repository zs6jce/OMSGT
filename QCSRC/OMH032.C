/* ----------------------------------------------------------------- *
 *                                                                   *
 * Parameters:      1 - Servername or IP-address                     *
 *                  2 - Portnumber                                   *
 *                  3 - Sender of e-mail                             *
 *                  4 - Receiver of e-mail                           *
 *                  5 - Blind Carbon Copy address                    *
 *                  6 - Subject of e-mail                            *
 *                  7 - File to send                                 *
 *                  8 - Helpdesk code                                *
 *                  9 - Status parameter (return parameter, the      *
 *                      address of the field should be given)        *
 *                                                                   *
 * Created by:      Remain Software                                  *
 * ----------------------------------------------------------------- *
 * Description:     The program takes the parameters and checks the  *
 *                  authorization and licences for the user. After   *
 *                  this the program creates a socket.               *
 *                  Next it connects to the smtp server. If this     *
 *                  went succesfull a smtp session is started        *
 *                  and the initial variables are set on the server  *
 *                  (sender, receiver). After this the file to send  *
 *                  is parsed and send line by line to the server.   *
 *                  The email is concluded by sending a dot to the   *
 *                  server.                                          *
 *                  The only way to know whether the program was     *
 *                  executed succesfully is to check the return      *
 *                  value of the status parameter. If successfull,   *
 *                  the value is *NORM otherwise the value is *TERM. *
 *                  During the smtp conversation, the server returns *
 *                  codes. If these codes do not equal 250, 220, 221 *
 *                  or 354 the program is also ended with *TERM      *
 * ----------------------------------------------------------------- */


#include <errno.h>
#include <stdio.h>
#include <netdb.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iconv.h>
#include <string.h>
#include <stdlib.h>
#include <recio.h>
#include <decimal.h>
#include <xxdtaa.h>

#pragma mapinc("MSX","OMS31HD/OMMSX(OMMSXR)","input","")
#include "MSX"

#pragma mapinc("MHX","OMS31HD/OMMHX(OMMHXR)","input","")
#include "MHX"

/* ----------------------------------------------------------------- *
 * Necessary for calling external OMH029 program in RPG              *
 * ----------------------------------------------------------------- */

#pragma map(check_authorization, "OMH029");
#pragma linkage(check_authorization, OS, nowiden);

void check_authorization(char *,
                         char *,
                         char *,
                         char *);

/* ----------------------------------------------------------------- *
 * Define standard values                                            *
 * ----------------------------------------------------------------- */

#define BUF_LEN 1024
#define MAX_LEN 128
#define REC_LEN 256
#define ARG_NUM 10

/* ----------------------------------------------------------------- *
 * global variables                                                  *
 * ----------------------------------------------------------------- */

int       sockfd;               /* socket variable                   */
int       len;                  /* length of string                  */
struct    sockaddr_in address;  /* variable with socket address info */
int       n;                    /* number of bytes read from server  */
char      str[BUF_LEN],         /* buffer for communication          */
          com[MAX_LEN],         /* command send to the server        */
          servername[MAX_LEN],  /* name of the server (or IP number) */
          portnumber[MAX_LEN],  /* number of the communication port  */
          sender[MAX_LEN],      /* email address of sender           */
          receiver[MAX_LEN],    /* email address of receiver         */
          Bcc[MAX_LEN],         /* blind carbon copy address         */
          subject[MAX_LEN],     /* subject of the e-mail             */
          filename[MAX_LEN],    /* name of the file to send          */
          record[BUF_LEN],      /* buffer for file communication     */
          helo[MAX_LEN],        /* internet address of the server    */
          hd_code[MAX_LEN],     /* help desk code                    */
          sTemp[MAX_LEN];       /* temporary string variable         */
_RFILE    *fp;                  /* file pointer to file to send      */
_XXOPFB_T *opfb;                /* file information structure        */
_RIOFB_T  *fb;                  /* file information structure        */
OMS31HD_OMMSX_OMMSXR_i_t ip;   /* information structure             */
OMS31HD_OMMHX_OMMHXR_i_t ih;   /* information structure             */
_RFILE    *om_msx;
_RFILE    *om_mhx;

int       ccsi=33;              /* input CCSID for conversion        */
int       ccso=33;              /* output CCSID for conversion       */

/* ----------------------------------------------------------------- *
* ASCII EBCDIC converter, modified code from IBM.                    *
* option  1 = from ascii to ebcdic.                                  *
* option !1 = from ebcdic to ascii.                                  *
* ------------------------------------------------------------------ */

void strcspc(char *str)
{
   int i;

   for (i=strlen(str)-1; i >= 0; i--)
   {
      if (str[i] == ' ') str[i] = '\0';
      else break;
   }

   return;
}

int AEC(int option, char ibuf[1024])
{
    char f_code[33];              /* From CCSID                           */
    char t_code[33];              /* To CCSID                             */
    iconv_t iconv_handle;         /* Conversion Descriptor returned       */
                                  /* from iconv_open() function           */
    size_t ibuflen;               /* Length of input buffer               */
    size_t obuflen;               /* Length of output buffer              */
    char *obuf;                   /* Pointer to output buffer             */
    char *isav;                   /* Saved pointer to input buffer        */
    char *osav;                   /* Saved pointer to output buffer       */

   /* -------------------------------------------------------------- *
    * Fill parameters depending on option.                           *
    * Open handle.                                                   *
    * -------------------------------------------------------------- */

    memset(f_code,'\0',33);
    memset(t_code,'\0',33);
    if(option == 1)
    {
    strcpy(f_code,"IBMCCSID008190000000");
    strcpy(t_code,"IBMCCSID00037");
    }

    if(option != 1)
    {
    strcpy(f_code,"IBMCCSID000370000000");
    strcpy(t_code,"IBMCCSID00819");
    }

    iconv_handle = iconv_open(t_code, f_code);
    if (iconv_handle.return_value < 0)
    {
        perror("iconv-handle");
        return(-1);
    }

   /* -------------------------------------------------------------- *
    * We pass 1k buffers.                                            *
    * -------------------------------------------------------------- */
    ibuflen = strlen(ibuf) + 1;
    obuflen = ibuflen;
    obuf    = (char *) malloc(obuflen);

   /* -------------------------------------------------------------- *
    * Save pointers & convert                                        *
    * -------------------------------------------------------------- */
    isav = ibuf;
    osav = obuf;

    iconv(iconv_handle, &ibuf, &ibuflen, &obuf, &obuflen);
    if(errno)
    {
        iconv_close(iconv_handle);
        perror("iconv");
        return(-1);
    }

   /* -------------------------------------------------------------- *
    * Fill our buffer & return.                                      *
    * -------------------------------------------------------------- */
    ibuf = isav;
    strcpy(ibuf, osav);

    iconv_close(iconv_handle);
    return(0);
}

/* ----------------------------------------------------------------- *
 * send a string to the server                                       *
 * ----------------------------------------------------------------- */

void send_server()
{
   strcspc (str);
   AEC(2, str);
   strcat(str, "\x0D\x0A\x00");
   write(sockfd, &str, strlen(str));
}

/* ----------------------------------------------------------------- *
 * read a string from the server                                     *
 * ----------------------------------------------------------------- */

int read_server()
{
   n = read(sockfd, &str, sizeof(str));
   AEC(1, str);
   str[3] = '\0';

  /* --------------------------------------------------------------- *
   * return the code from the server to the main procedure           *
   * --------------------------------------------------------------- */
   return atoi(str);
}

/* ----------------------------------------------------------------- *
 * mainline for sending mail via a file                              *
 * ----------------------------------------------------------------- */

void prterror(char *txt, int cd, int how)
{
   printf("servername = %s\n", servername);
   printf("portnumber = %s\n", portnumber);
   printf("sender     = %s\n", sender);
   printf("receiver   = %s\n", receiver);
   printf("bcc        = %s\n", Bcc);
   printf("subject    = %s\n", subject);
   printf("filename   = %s\n", filename);
   printf("hd_code    = %s\n", hd_code);
   if (how == 1)
   {
      printf("command    = %s\n", txt);
      printf("error code = %d\n", cd);
   }
   else if (how == 0)
      printf("ERROR      = %s\n", txt);
   perror("Foutmelding gegenereerd door het systeem:\n");

   return;
}

void main(int argc, char *argv[])
{
   time_t ltime;     /* variable needed for time purposes.           */

  /* --------------------------------------------------------------- *
   * check number of parameters, if wrong, exit the application      *
   * --------------------------------------------------------------- */
   if (argc != ARG_NUM)
   {
      printf("Number of Parameters not correct\n\nUsage:\n\n", n);
      printf("1 - Servername or IP-address\n");
      printf("2 - Portnumber\n");
      printf("3 - Sender of e-mail\n");
      printf("4 - Receiver of e-mail\n");
      printf("5 - Blind Carbon Copy address\n");
      printf("6 - Subject of e-mail\n");
      printf("7 - File to send\n");
      printf("8 - Helpdesk code\n");
      printf("9 - Status parameter (return parameter, the\n"
             "    address of the field should be given)\n\n");
      printf("number of parameters given: %d\n", argc);
      exit(1);
   }
   else
   {
      strcpy(servername, argv[1]);
      strcpy(portnumber, argv[2]);
      strcpy(sender,     argv[3]);
      strcpy(receiver,   argv[4]);
      strcpy(Bcc,        argv[5]);
      strcpy(subject,    argv[6]);
      strcpy(filename,   argv[7]);
      strncpy(hd_code, argv[8], sizeof(ih.HXHEDC));
      hd_code[sizeof(ih.HXHEDC)] = '\0';

     /* ------------------------------------------------------------ *
      * following parameter has to be 5 long for RPG puposes         *
      * ------------------------------------------------------------ */
      strncpy(argv[ARG_NUM-1], "*NORM", 5);

     /* ------------------------------------------------------------ *
      * check authorization and licences for hd_code                 *
      * ------------------------------------------------------------ */
      check_authorization("2", hd_code, "0", argv[ARG_NUM-1]);
      if (strncmp(argv[ARG_NUM-1], "*NORM", 5) != 0)
      {
         strncpy(argv[ARG_NUM-1], "*TERM", 5);
         prterror("Authorization not granted", 0, 0);
         exit(1);
      }
   }

   if ((om_msx = _Ropen("*LIBL/OMMSX",
                        "rr, arrseq=Y")) != NULL)
   {
     /* ------------------------------------------------------------ *
      * determine address for helo command by reading file OMMSX     *
      * ------------------------------------------------------------ */
      fb = _Rreadf(om_msx, &ip,
              sizeof(OMS31HD_OMMSX_OMMSXR_i_t), __DFT);
      strncpy(sTemp, ip.SXDOMC, sizeof(ip.SXDOMC));
      sTemp[sizeof(ip.SXDOMC)] = '\0';
      sprintf(helo, "helo %s", sTemp);

      if ((strcmp(servername, "*DFT") == 0) ||
          (strcmp(portnumber, "*DFT") == 0))
      {
         strncpy(sTemp, ip.SXPORT, sizeof(ip.SXPORT));
         sTemp[sizeof(ip.SXPORT)] = '\0';
         strcpy(portnumber, sTemp);

         strncpy(sTemp, ip.SXIPAD, sizeof(ip.SXIPAD));
         sTemp[sizeof(ip.SXIPAD)] = '\0';
         strcpy(servername, sTemp);
      }
   }
   _Rclose(om_msx);

   if (strcmp(sender, "*DFT") == 0)
   {
      if ((om_mhx = _Ropen("*LIBL/OMMHXL1",
                           "rr")) != NULL)
      {
         fb = _Rreadk(om_mhx, &ih,
                 sizeof(OMS31HD_OMMHX_OMMHXR_i_t),
                 __KEY_EQ, hd_code, sizeof(ih.HXHEDC));
         if (fb->num_bytes > 0)
         {
            strncpy(sTemp, ih.HXEMAC, sizeof(ih.HXEMAC));
            sTemp[sizeof(ih.HXEMAC)] = '\0';
            strcpy(sender, sTemp);
            strcspc(sender);
         }
      }
      _Rclose(om_mhx);
   }

  /* --------------------------------------------------------------- *
   * create a new socket                                             *
   * --------------------------------------------------------------- */
   if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
   {
     /* ------------------------------------------------------------ *
      * error occurred so quit application, set status to *TERM      *
      * ------------------------------------------------------------ */
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror("no socket created", 0, 0);
      exit(1);
   }

  /* --------------------------------------------------------------- *
   * set values for socket connection                                *
   * --------------------------------------------------------------- */
   address.sin_family = AF_INET;
   address.sin_addr.s_addr = htonl(inet_addr(servername));
   address.sin_port = htons((u_short) atoi(portnumber));
   len = sizeof(address);

  /* --------------------------------------------------------------- *
   * connect to the smtp server                                      *
   * --------------------------------------------------------------- */
   if (connect(sockfd, (struct sockaddr *)&address, len) < 0)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror("no connection to SMTP server", 0, 0);
      exit (1);
   }

  /* --------------------------------------------------------------- *
   * get first line from server                                      *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror("get first line from server", n, 1);
      exit(1);
   }

  /* --------------------------------------------------------------- *
   * send helo to smtp - server                                      *
   * --------------------------------------------------------------- */
   strcpy(str, helo);
   strcpy(com, str);
   send_server();

  /* --------------------------------------------------------------- *
   * read comments from server                                       *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror(com, n, 1);
      exit(1);
   }

  /* --------------------------------------------------------------- *
   * send sender to server                                           *
   * --------------------------------------------------------------- */
   sprintf(str, "mail from:<%s>", sender);
   strcpy(com, str);
   send_server();

  /* --------------------------------------------------------------- *
   * read comments from server                                       *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror(com, n, 1);
      exit(1);
   }

  /* --------------------------------------------------------------- *
   * send receiver to server                                         *
   * --------------------------------------------------------------- */
   sprintf(str, "rcpt to:<%s>", receiver);
   strcpy(com, str);
   send_server();

  /* --------------------------------------------------------------- *
   * read comments from server                                       *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror(com, n, 1);
      exit(1);
   }

   if (strcmp (Bcc, "") != 0)
   {
      sprintf(str, "rcpt to:<%s>", Bcc);
      strcpy(com, str);
      send_server();

     /* ------------------------------------------------------------ *
      * read comments from server                                    *
      * ------------------------------------------------------------ */
      n = read_server();
      if (n != 250 && n != 220 && n != 221 && n != 354)
      {
         strncpy(argv[ARG_NUM-1], "*TERM", 5);
         close(sockfd);
         prterror(com, n, 1);
         exit(1);
      }
   }

  /* --------------------------------------------------------------- *
   * send data to the server                                         *
   * --------------------------------------------------------------- */
   strcpy(str, "data");
   strcpy(com, str);
   send_server();

  /* --------------------------------------------------------------- *
   * read comments from server                                       *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror(com, n, 1);
      exit(1);
   }

  /* --------------------------------------------------------------- *
   * send standard line to server                                    *
   * --------------------------------------------------------------- */
   time(&ltime);
   sprintf(str, "Date: %s", ctime(&ltime));
   send_server();
   sprintf(str, "From: %s", sender);
   send_server();
   sprintf(str, "Subject: %s", subject);
   send_server();
   sprintf(str, "To: %s", receiver);
   send_server();
   if (strcmp (Bcc, "") != 0)
   {
      sprintf(str, "Bcc: %s", Bcc);
      send_server();
   }
   sprintf(str, "");
   send_server();

  /* --------------------------------------------------------------- *
   * send data part to the server                                    *
   * --------------------------------------------------------------- */
   if ((fp = _Ropen (filename, "rr+, arrseq=Y")) == NULL)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror("not able to open filename", 0, 0);
      exit(1);
   }
   else
   {
      opfb = _Ropnfbk(fp);

      /* ----------------------------------------------------------- *
       * read the first record in the file                           *
       * ----------------------------------------------------------- */
      fb = _Rreadf(fp, &record, REC_LEN, __DFT);

      /* ----------------------------------------------------------- *
       * keep track of which record we are handling                  *
       * ----------------------------------------------------------- */
      while (fb->num_bytes != EOF)
      {
         /* -------------------------------------------------------- *
          * set the current string to send                           *
          * -------------------------------------------------------- */
         strcpy (str, record+12);

         strcpy (sTemp, str);
         strcspc (sTemp);
         if (strcmp(sTemp, ".") == 0)
            strcpy(str, "..");

         send_server();
         fb = _Rreadn(fp, &record, REC_LEN, __DFT);
      }

      _Rclose(fp);
   }

  /* --------------------------------------------------------------- *
   * send empty line to server                                       *
   * --------------------------------------------------------------- */
   strcpy(str, "");
   send_server();

  /* --------------------------------------------------------------- *
   * send dot to server                                              *
   * --------------------------------------------------------------- */
   strcpy(str, ".");
   strcpy(com, str);
   send_server();

  /* --------------------------------------------------------------- *
   * read data from the server                                       *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror(com, n, 1);
      exit(1);
   }

  /* --------------------------------------------------------------- *
   * send quit to the server                                         *
   * --------------------------------------------------------------- */
   strcpy(str, "quit");
   strcpy(com, str);
   send_server();

  /* --------------------------------------------------------------- *
   * read comments from server                                       *
   * --------------------------------------------------------------- */
   n = read_server();
   if (n != 250 && n != 220 && n != 221 && n != 354)
   {
      strncpy(argv[ARG_NUM-1], "*TERM", 5);
      close(sockfd);
      prterror(com, n, 1);
      exit(1);
   }

   close (sockfd);
}
