#include "stdio.h"
#include "zipdefs.h"
#include "struct.h"
#include "xzipvers.h"
/* EZIP should include ezipdefs.h */


/************************************************************************
*									*
*	G L O B A L S							*
*									*
************************************************************************/

/*  M E M O R Y   R E L A T E D  */

char main_font[2048];
char *alt_font = &main_font[1024];

int printabt = 0;
int diskabt = 0;
struct devhead cridevi;

long datend;
int fontlen;

ZIPCHAR *dataspace;	/* data space pointer; where code lives */ 
short memreq;			/* total dataspace requested, in blocks */
int no_paging = 0;	/* 1 if everything loaded */
int swap_to_floppy = 0;
int swapped = 0;
int isave_allowed = 1;
ZIPCHAR *isave_buffer;
int isave_happened = 0;
short isave_size = 0;

/*  F I L E   C H A N N E L S   A N D   D E S C R I P T O R S  */

int tty_busted = 0;
int tandy = -1;
int ega = 0;

int gamechn = 0;
int savechn;		/* file channel storage */
int scrptfd;			/* file for scripting */
int swapdrive;
int floppy_script = 0;

char *gamfile,
     rgamfile[PATHSIZ],
     savfile[PATHSIZ],
     defdrive[3];

/*  I / O  B U F F E R S  */

char *chrptr, *endbuf,		/* output buffer pointers */
     outbuf[OBUFSIZ],		/* maximum output buffer */
     inbuf[IBUFSIZ];		/* maximum input buffer */
int chars_on_line = 0;		/* chars since CR */

/*  F L A G S  */

char scripting = 0,		/* scripting flag */
     scrchk = 0,		/* flag to check for script bit */
     slflg = 1,			/* status line in place (EZIP) */
     screen,			/* current screen for output */
     spltflg = 0;		/* screen split flag--# lines in screen 1, =
				   top line of screen 0.  0 means unsplit */
    char bufflg;	/* buffering output to console */
    char prevbuf;	/* to remember if we were buffering */
    char vidflg = 1;	/* outputting to screen if 1 */
    char scrhld;	/* temporarily block scripting if 1 */
    char topscr = 0;
    char fontsize = 1;
#ifdef GERMAN
    int language_ok = 1;
#endif
    char fgcolor = DEF_FG_COLOR;	/* current foreground color */
    char bgcolor = DEF_BG_COLOR;	/* current background color */
    char zip_to_ibm_color[] = {-1, -2, 0, 4, /* nc, def, black, red */
				     2, 14, 1, 5, /* green, ylw, blue, mag */
				     3, 7};	  /* cyan, white */
    char ibm_to_zip_color[] = {2, 6, 4, 8, 3, 7,
				     5, 9, 9, 6, 4, 8,
				     3, 7, 5, 9};
int mouseflg;   /* mouse flag  0 not installed */
int mouse_event_count=0;          /* number of events in mouse queue */
int mouse_queue_start=0;	/* points to next place to read from */
int mouse_event[MOUSE_EVENT_LENGTH];
int mouse_x[MOUSE_EVENT_LENGTH];
int mouse_y[MOUSE_EVENT_LENGTH];
int mouse_buttons;
int mouse_cur_x;
int mouse_cur_y;
int mouse_frob[3];
char intwrd[] = {MACHID, INTVER};

/*  T T Y   */

int ttyfd,			/* for setting up terminal i/o */
    ttysav;			/* storage of a startup tty condition */
int winlen = 22,		/* window length */
    linecnt;			/* line count for MORE */
int LM = 1;
int RM = 80;
int scrwid = 79;
int curscrwid = 79;
int dosprint = 0;		/* script using bios normally */
int docolor = 0;
int mouse_on = 0;		/* don't use mouse unless asked */
char oldmode = 0;
char scrmode = 0;
char normattr = 0x07;
char curattr = 0x07;
int graphics = 0;
char highlight_bits = XMODE;
char current_highlight = 0;
char scr0x = 0;
char scr0y = 0;
int scr0font = 1;
char scr1x = 0;
char scr1y = 0;
int scr1font = 1;
char scrx = 0;
char scry = 0;
int scrfont = 1;

char slpp = SCREEN_LENGTH;	/* lines per page */

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
    main_vocabulary;		/* primary vocabulary table...*/

unsigned char character_set[78] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ \n0123456789.,!?_#'\"/\\-:()";


char read_terminator = 0;

    		/* for OPDIROUT */
    char rdir;			/* 1 if outputting to a table */
    ZIPINT rtable,		/* pointer to table */
    rtable2,			/* pointer to place for next chr in table */
    rdirout;			/* number of chrs output to table */

/*  O P R E A D   G L O B A L S  */ 

ZIPCHAR *curword;		/* ascii input word pointer */
ZIPCHAR *rdbos;			/* beginning of read string (table 1) */
ZIPCHAR *nxttok;		/* pointer to next input word */

char *lastbrk,			/* pointer to last ascii break char */
     *esibrks,			/* end of self-inserting break chars */
     rbrks[32],			/* string of read break chars */
     irbrks[] = {SPACE,TAB,CR,FF,PERIOD,COMMA,QMARK,NULL};
				/* ^^ initial read break chars */
     
short rdwstr[CHRS_PER_ZWORD/3],	/* vocabulary word repository during lookup */
      rdeos,			/* read end of string (offset) */
      curoff;			/* current offset in ascii input buffer */

/*  R A N D O M  */

int rseed;			/* seed for random numbers */

int rcycle;			/* holds upper bound for non-random cycles */
int rconst;			/* holds 1 to <rcycle> */

/*  P A G I N G  */

short last_block_size = 0;
short game_blocks = 0;

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

ZIPINT zlocs;			/* pointer to z local variables */
char zargct;
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
