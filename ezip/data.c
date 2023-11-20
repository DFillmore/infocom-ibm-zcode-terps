#include "stdio.h"
#include "malloc.h"
#include "zipdefs.h"
#include "struct.h"
/* EZIP should include ezipdefs.h */


/************************************************************************
*									*
*	G L O B A L S							*
*									*
************************************************************************/

/*  M E M O R Y   R E L A T E D  */

ZIPCHAR *dataspace;	/* data space pointer; where code lives */ 
short memreq;			/* total dataspace requested, in blocks */

/*  F I L E   C H A N N E L S   A N D   D E S C R I P T O R S  */

int gamechn, savechn;		/* file channel storage */
FILE *scrptfd;			/* file for scripting */

char *gamfile, 
     gamfbuf[PATHSIZ],
     savfile[PATHSIZ];

/*  I / O  B U F F E R S  */

char *chrptr, *endbuf,		/* output buffer pointers */
     *p_chrptr, *p_endbuf,	/* pipe output buffer pointers */
     p_outbuf[PBUFSIZ],		/* pipe output buffer */ 
     outbuf[OBUFSIZ],		/* maximum output buffer */
     inbuf[IBUFSIZ];		/* maximum input buffer */
int chars_on_line = 0;		/* chars since CR */

/*  F L A G S  */

char scripting = 0,		/* scripting flag */
     scrchk = 0,		/* flag to check for script bit */
     slflg = 1,			/* status line in place (EZIP) */
     scroll = 1,		/* windowed scrolling */
     screen,			/* current screen for output */
     splitable = 1,		/* ablity to support split screen */
     spltflg = 0,		/* screen split flag--# lines in screen 1, =
				   top line of screen 0.  0 means unsplit */
#ifdef ZIP
     toplin = 0,
#endif
     quit = 0;			/* flag that says quit */
     end_loop = 0;		/* flag that says break out of main_loop */

#ifdef EXZIP
    char bufflg;	/* buffering output to console */
    char prevbuf;	/* to remember if we were buffering */
    char vidflg = 1;	/* outputting to screen if 1 */
    char scrhld;	/* temporarily block scripting if 1 */
    char topscr = 0;
#endif


#ifdef ZIP
char intwrd[] = {'\0', INTVER};
#endif
#ifdef EXZIP
char intwrd[] = {MACHID, INTVER};
#endif

/*  T T Y   */

int ttyfd,			/* for setting up terminal i/o */
    ttysav;			/* storage of a startup tty condition */
int winlen = 22,		/* window length */
    linecnt;			/* line count for MORE */
int LM = 1;
int RM = 80;
int scrwid = 79;
int docolor = 0;
char oldmode = 0;
char scrmode = 0;
char normattr = 0x07;
char curattr = 0x07;
int graphics = 0;
#ifdef EZIP
char highlight_bits = EMODE;
#else
char highlight_bits = XMODE;
#endif
char current_highlight = 0;
char scr0x = 0;
char scr0y = 0;
char scr1x = 0;
char scr1y = 0;
char scrx = 0;
char scry = 0;

#ifdef EXZIP
char slpp = SCREEN_LENGTH;	/* lines per page */
#endif

/*  T A B L E   P O I N T E R S  */

ZIPINT timemd,			/* time/score mode */
    zorkid,			/* game id */
    endlod,			/* endlod pointer */
    voctab,			/* vocabulary table ptr */
    objtab,			/* object table ptr */
    glotab,			/* global table ptr */
    wrdtab,			/* word table ptr */
    wrdoff,			/* offset to current wrdtab */
    purbot,			/* pure load ptr */
    vwlen,			/* number of bytes in vocab word entry */
    vwords,			/* number of word entries in vocab table */
    vocbeg;			/* beginning of actual vocabulary */

#ifdef EXZIP
    		/* for OPDIROUT */
    char rdir;			/* 1 if outputting to a table */
    ZIPINT rtable,		/* pointer to table */
    rtable2,			/* pointer to place for next chr in table */
    rdirout;			/* number of chrs output to table */
#endif

/*  O P R E A D   G L O B A L S  */ 

ZIPCHAR *curword;		/* ascii input word pointer */
ZIPCHAR *rdbos;			/* beginning of read string (table 1) */
ZIPCHAR *nxttok;		/* pointer to next input word */

char *lastbrk,			/* pointer to last ascii break char */
     *esibrks,			/* end of self-inserting break chars */
     rbrks[32],			/* string of read break chars */
     irbrks[] = {SPACE,TAB,CR,FF,PERIOD,COMMA,QMARK,NULL},
				/* ^^ initial read break chars */
     zchars[] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ  0123456789.,!?_#\'\"/\\-:()";

short rdwstr[CHRS_PER_ZWORD/3],	/* vocabulary word repository during lookup */
      rdeos,			/* read end of string (offset) */
      curoff;			/* current offset in ascii input buffer */

/*  R A N D O M  */

int rseed;			/* seed for random numbers */

#ifdef EXZIP
int rcycle;			/* holds upper bound for non-random cycles */
int rconst;			/* holds 1 to <rcycle> */
#endif

/*  P A G I N G  */

short zpc1,			/* z program counter block number */
    zpc2,			/* z program counter byte offset */
    zblk,			/* roving zpointer (bsplit/getbyt/putstr), */
    zoff,			/*   usually different than zpc */
    curblk = -1,		/* current block (same as last zpc1) */
    curpag = -1;		/* last page gotten (from getpag) */

ZIPCHAR *curblkloc;		/* pointer to curblk block */
ZIPCHAR *curpagloc;		/* pointer to curpag block */

struct blkdesc
    pagedesc[MAXBLKS],		/* one descriptor for each virtual page */
    *mru,			/* most recently used blkdesc */
    *pagemap[MAXBLKS];		/* one mapping for each virtual page */

/*  Z - S T A C K   A N D   S Y S T E M   S T A C K  */

ZIPINT zstack[LSTACK],		/* z stack and stack pointer */
    *zsp;
int sstack[STKLEN],		/* system stack and stack pointer */
    *ssp;

ZIPINT zlocs;			/* pointer to z local variables */
short argblk[MAXARGS];		/* argument block (all opcodes) */

/*  D E B U G G I N G  */

#if _DEBUG
char debug = 0;			/* debug flag */
struct ops ins_tbl[256];
char opstrs[256][8];		/* opcode names */
int skipcnt = 0;		/* skip n instructions */
int bfunc;
short z1, z2;			/* for setting breakpoints at ZPCn */
struct history_list op_hist[16];
int last_ins = 0;
#endif
