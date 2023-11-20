/************************************************************************
*                                                                       *
*   E R R O R   H A N D L I N G                                         *
*                                                                       *
************************************************************************/
#include <stdlib.h>
#include <ctype.h>
#include <dos.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"

void fatal(message)
char *message;
{  /*   Fatal gives the standard zip fatal error message (message passed)
   by caller and then performs clean up through Z_EXIT.
   */
    md_ttyres();         /* reset the tty please! */

    md_outstr( "\nFatal error:\n" );
    md_outstr( message );
	 md_outstr( "\nPlease hit [RETURN] to end.\n" );
    md_getch();
    z_exit( -1 );         /* exit after clean up */
}

void z_exit( ecode )
   int ecode;
{  /*   Z_exit reset the tty before exit.  If the tty is not reset, the
   user will be logged out on exit.
   */
   md_ttyres();         /* reset the tty please! */
   if (Scrptfd > 0)
      md_close_script( Scrptfd );

	hfree( Dataspace );
	hfree( (char huge *)pagedesc );
	hfree( (char huge *)pagemap );
	hfree( Pic_buff );
	hfree( Hash_buff );
   exit( ecode );
}

/* Call from critical error handler with code (from DI) as only argument.
   returns 0, 1, 2, 3.  Does 3 work on smaller dos versions??? */

#define NMCRITS 12

char *err_names[] = {"Disk write-protected",
               "Unknown unit",
               "Not ready",
               "Unknown command",
               "Data error",
               "Bad drive request",
               "Seek error",
               "Unknown media type",
               "Sector not found",
               "Out of paper",
               "Write fault",
               "Read fault",
               "General failure",
               "Unknown error -- number "};

#define ABORT 0
#define RETRY 1

int do_error(dev_header, pax, pdi)
	int dev_header;
   int pax, pdi;
{
   int errnum = pdi & 0xff;
   int diskerr = (pax >= 0);
   int retval;
   char inchr;

	if ( !No_abort )
	{ /* didn't expect this to happen, so ask user what next */
   	md_sound(FEEP);
   	md_mcrlf();
   	if (!diskerr && (errnum == 2))
   	{   
      	errprnt("Printer not ready");
   	}   
   	else
   	{   
      	if (errnum > NMCRITS)
      	{   
         	errprnt(err_names[NMCRITS + 1]);
         	print_number(errnum,md_putc);
      	}   
      	else
      	{   
         	errprnt(err_names[errnum]);
         	errprnt(" on ");
         	if (diskerr)  
         	{
            	md_putc((char)('A'+(pax & 7)));
            	md_putc(':');
            	if (Scripting && (Win_attributes & SCRIPTMASK))
            	{
               	md_script_char((char)('A'+(char)(pax & 7)));
               	md_script_char(':'); 
            	}     
         	}
         	else
            	errprnt("printer"); 
      	}
   	}   
   	errprnt(". ");
   	do
   	{ /* ask user what to do next */
      	errprnt("A to abort, R to retry: ");
      	inchr = lc( (char)md_inp() );
      	if ( isprint( inchr ) )
      	{ /* this here character is printable, so show the user */
         	md_putc( inchr );
      	}
      	md_mcrlf();

      	if (inchr != 'a' && inchr != 'r')
      	{ /* user choked, so complain and try again */
         	md_sound(FEEP);
      	}
   	} while ( inchr != 'a' && inchr != 'r' );

   	if ( inchr == 'r' )
   	{ /* just signal retry */
      	retval = RETRY;
   	}
   	else
   	{ /* must be abort, so which one are we aborting */
      	retval = ABORT;
      	if ( diskerr )
         	Diskabt++;
      	else
         	Printabt = 1;
   	}
	}
	else
	{ /* figgered this might happen, so just abort */
		retval = ABORT;
	}

   return(retval);
}

