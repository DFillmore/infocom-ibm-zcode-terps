/************************************************************************
*									*
*	E R R O R   H A N D L I N G 					*
*									*
************************************************************************/
#include "zipdefs.h"
#include "struct.h"
#include "extern.h"

fatal(message)
char *message;
{  /*	Fatal gives the standard zip fatal error message (message passed)
	by caller and then performs clean up through Z_EXIT.
   */
#if _DEBUG
    int i, j, k;
#endif

    md_ttyres();
    printf("\nFatal error: %s\n", message);
#if _DEBUG
    if (debug) THEN {
      printf("\nStrike any key to get history list");
      while (getchar() == -1)
        ;
      printf("\nZPC1:ZPC2   Opcode   Args");
      j = ++last_ins & 15;
      for (i = 0; i <= 15; i++) {
	printf("\n%04.4x:%04.4x   %s   ", op_hist[j].z1, op_hist[j].z2, op_hist[j].opstring);
	for (k = 0; k < MAXARGS; k++)
	  printf("%04.4x ", op_hist[j].argblk[k]);
	j = ++j & 15;
	}
      printf("\n");
      }
#endif
    z_exit();			/* exit after clean up */
}

z_exit()
{  /*	Z_exit reset the tty before exit.  If the tty is not reset, the
	user will be logged out on exit.
   */
    md_ttyres();			/* reset the tty please! */
    exit();
}
