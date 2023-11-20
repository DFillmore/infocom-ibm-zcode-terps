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
    md_mcrlf();
    errprnt("Fatal error: ");
    md_mcrlf();
    errprnt(message);
#if _DEBUG
    if (debug) THEN {
      printf("Strike any key to get history list");
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
    if (scrptfd > 0)
      md_close_script();
    exit();
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

#define IGNORE 0
#define RETRY 1

int do_error(headp, pax, pdi)
struct devhead *headp;
int pax, pdi;
{
  int md_putc();
  int errnum = pdi & 0xff;
  int diskerr = (pax >= 0);
  int retval;
  char inchr;
  md_sound(FEEP);
  md_mcrlf();
  if (!diskerr && (errnum == 2)) THEN {
    errprnt("Printer not ready"); }
   else {
    if (errnum > NMCRITS) THEN {
      errprnt(err_names[NMCRITS + 1]);
      print_number(errnum,md_putc); }
     else {
      errprnt(err_names[errnum]);
      errprnt(" on ");
      if (diskerr) THEN {
        md_putc('A'+(pax & 7));
	md_putc(':');
	if (scripting && (screen == 0)) {
	  md_script_char('A'+(pax & 7));
	  md_script_char(':'); } }
       else
	errprnt("printer"); } }
  errprnt(". ");
  while (ZTRUE) {
    errprnt("A to abort, R to retry: ");
    inchr = md_inp();
    if (inchr <= 'z' && inchr >= 'a')
      inchr += 'A' - 'a';
    if (inchr == 'A' || inchr == 'R') break;
    md_mcrlf();
    md_sound(FEEP); }
  md_putc(inchr);
  md_mcrlf();
  if (diskerr) {
    if (inchr == 'A') {
      diskabt++;
      retval = IGNORE; }
     else
      retval = RETRY; }
   else {
    if (inchr == 'A') {
      printabt = 1;
      retval = IGNORE; }
     else
      retval = RETRY; }
  return(retval);
}

