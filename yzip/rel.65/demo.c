.bm56
.tm4
.lm2
/* demo.c - demonstrate critical error handler */

#include <stdio.h>
#include <dos.h>
#include <ctype.h>
#include "ce.h"

#define display(s)  bdosptr(9, s, 0)
#define getkey()    bdos(1, 0, 0)
#define errmsg()    printf("I/O Error detected\n")

int CriticalError;

char *err_msg[] = {
     "Write-protected disk",
     "Unknown Unit: invalid drive number",
     "Device not ready",
     "Unknown command request",
     "CRC error",
     "Bad request structure length",
     "Seek error: move to requested track failed",
     "Unknown media: disk format not recognized",
     "Sector not found",
     "Printer out of paper",
     "Write error",
     "Read error",
     "General error",
};

int far asker(int axreg, int direg, int bpreg, int sireg)
{
     /* This function is called whenever a critical error
      * error occurs.  axreg, direg, bpreg and sireg are
      * the exact register values DOS sets up whenever it
      * invokes interrupt 24H.
      * This function should return 'A' to Abort, 'R' to
      * Retry, 'I' to Ignore, or 'C' to Cancel operation
      * and return to program with an error code.
      */

     char buf[60];
     struct devhdr far *hdr;
     AXBITS ax;
     DIBITS di;
     int c;

     hdr = MK_FP(bpreg, sireg);
     ax.ax = axreg;
     di.di = direg;

     display("Critical Error:\r\n$");
     sprintf(buf, "\tAX:%04x DI:%04x BP:%04x SI:%04x\n\r$",
       axreg, direg, bpreg, sireg);
     display(buf);
     if (ax.parts.typ == 0) {
          display("\tDisk Error\n\r$");
          sprintf(buf, "\tDrive %c:\n\r$", 'A' + ax.parts.drive);
          display(buf);
     } else if (!(hdr->dh_attr & (short) 0x8000)) {
          display("\tFAT Error\n\r$");
          sprintf(buf, "\tDrive %c:\n\r$", 'A' + ax.parts.drive);
          display(buf);
     } else {
          display("\tNon-Disk Error\n\r$");
          sprintf(buf, "\tDevice %.8Fs\n\r$", hdr->dh_name);
          display(buf);
     }

     if (ax.parts.typ == 0) {
          if (ax.parts.r_w == 0)
               display("\tRead error\n\r$");
          else
               display("\tWrite error\n\r$");

          display("\tError involved $");
          switch (ax.parts.area) {
          case 0:   display("DOS system files$");      break;
          case 1:   display("FAT$");                   break;
          case 2:   display("directory$");             break;
          case 3:   display("data area of disk$");     break;
          }
          display("\r\n$");
     }

     if (di.parts.errorcode < 0 || di.parts.errorcode > 0xc)
          display("\tUnknown error code\n\r$");
     else {
          sprintf(buf, "\t%s\n\r$", err_msg[di.parts.errorcode]);
          display(buf);
     }

     display("Retry or Cancel Operation? (R/C) $");
     while ((c = toupper(getkey())) != 'C' && c != 'R')
          ;
     display("\r\n$");
     if (c == 'C')
          CriticalError = 1;
     return(c);
}

main()
{
     FILE *f;
     int i;

     setCEasker(asker);            /* install asker function */
     setvect(CE_INTERRUPT, CE_trap);    /* replace vector 24H */

     printf("Critical Error Handler and Asker Test\n\n");

     printf("About to send string to printer.\n");
     printf("Please put printer off-line for test and hit a key...");
     getkey();
     printf("\n");

     if ((f = fopen("PRN", "w")) == NULL) {
          errmsg();
          return;
     } else {
          CriticalError = 0;
          i = fprintf(f, "a longer string a longer string");
          if (i == EOF || CriticalError)
               errmsg();
          printf("ferror(f) == %d\n", ferror(f));
          printf("printed %d chars\n", i);
          CriticalError = 0;
          if (fflush(f) || CriticalError)
               errmsg();
          CriticalError = 0;
          if (fclose(f) || CriticalError)
               errmsg();
     }

     printf("\nAbout to access drive A:\n");
     printf("Please remove disk from drive and hit a key...");
     getkey();
     printf("\n");
     if ((f = fopen("a:junk.jnk", "r")) == NULL)
          errmsg();
     else
          fclose(f);
}
