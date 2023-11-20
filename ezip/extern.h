/************************************************************************
*									*
*	E X T E R N S							*
*									*
************************************************************************/

#include "stdio.h"
#include "malloc.h"

/*  M E M O R Y   R E L A T E D  */

extern ZIPCHAR *dataspace;	/* data space pointer; where code lives */ 
extern short memreq;		/* total dataspace requested, in blocks */

/*  F I L E   C H A N N E L S   A N D   D E S C R I P T O R S  */

extern int gamechn, savechn;		/* file channel storage */
extern FILE *scrptfd;			/* file for scripting */

extern char *gamfile, 
     gamfbuf[],
     savfile[];

/*  I / O  B U F F E R S  */

extern char *chrptr, *endbuf,		/* output buffer pointers */
     *p_chrptr, *p_endbuf,	/* pipe output buffer pointers */
     p_outbuf[],		/* pipe output buffer */ 
     outbuf[],		/* maximum output buffer */
     inbuf[];		/* maximum input buffer */
extern int chars_on_line;	/* number of characters since last CR
				   in screen 0 */

/*  F L A G S  */

extern char scripting,		/* scripting flag */
     scrchk,			/* flag to check for script bit */
     slflg,			/* status line in place (EZIP) */
     scroll,			/* windowed scrolling */
     screen,			/* current screen for output */
     splitable,			/* ablity to support split screen */
     spltflg,			/* screen split flag -- top line of scr0 */
#ifdef ZIP
     toplin,
#endif
     quit,			/* game op flag for quit */
     end_loop;			/* break out of main_loop */

#ifdef EXZIP
extern char intwrd[];		/* machine id, interpreter version */
extern char bufflg, prevbuf;	/* buffering output to console */
extern char slpp,               /* length of screen in rows */
     topscr,
     vidflg,			/* is output directed to screen? */
     scrhld;			/* is scripting temporarily off? */
extern int graphics;		/* are we in graphics mode or alpha mode? */
#endif

/*  T T Y   */

extern int ttyfd,		/* for setting up terminal i/o */
    ttysav;			/* storage of a startup tty condition */
extern int winlen,		/* window length */
    linecnt;			/* line count for MORE */
extern int RM,LM;		/* margins */
extern int scrwid;		/* actual screen width */
extern int docolor;		/* -1 means no, 1 means yes, 0 means don't
  care.  color is currently not supported anyway */
extern char oldmode;		/* old screen mode, restore on quit */
extern char scrmode;		/* from INT 10 */
extern char normattr;		/* normal character attribute */
extern char curattr;		/* current attribute */
extern char highlight_bits;	/* what we can do */
extern char current_highlight;	/* what we're currently doing */
extern char scr0x,scr0y,scr1x,scr1y,scrx,scry;
/*  T A B L E   P O I N T E R S  */

extern ZIPINT timemd,		/* time/score mode */
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
    char rdir;			/* redirecting output to a table */
    ZIPINT rtable,		/* the table for redirecting output */
    rtable2,			/* pointer for next chr in table */
    rdirout;			/* count of chrs redirected */
#endif

/*  O P R E A D   G L O B A L S  */ 

extern ZIPCHAR *curword;	/* ascii input word pointer */
extern ZIPCHAR *rdbos;		/* beginning of read string (table 1) */
extern ZIPCHAR *nxttok;		/* pointer to next input word */

extern char
     *lastbrk,			/* pointer to last ascii break char */
     *esibrks,			/* end of self-inserting break chars */
     rbrks[],			/* string of read break chars */
     irbrks[],			/* ^^ initial read break chars */
     zchars[];

extern short rdwstr[],	/* vocabulary word repository during lookup */
      rdeos,			/* read end of string (offset) */
      curoff;			/* current offset in ascii input buffer */

/*  R A N D O M  */

extern int rseed;			/* seed for random numbers */

#ifdef EXZIP
int rcycle;			/* holds upper bound for non-random cycles */
int rconst;			/* holds 1 to <rcycle> */
#endif

/*  P A G I N G  */

extern short zpc1,		/* z program counter block number */
    zpc2,			/* z program counter byte offset */
    zblk,			/* roving zpointer (bsplit/getbyt/putstr), */
    zoff,			/*   usually different than zpc */
    curblk,			/* current block (same as last zpc1) */
    curpag;			/* last page gotten (from getpag) */

extern ZIPCHAR *curblkloc;	/* pointer to curblk block */
extern ZIPCHAR *curpagloc;	/* pointer to curpag block */

extern struct blkdesc
    pagedesc[],			/* one descriptor for each virtual page */
    *mru,			/* most recently used blkdesc */
    *pagemap[];			/* one mapping for each virtual page */

/*  Z - S T A C K   A N D   S Y S T E M   S T A C K  */

extern ZIPINT zstack[],		/* z stack and stack pointer */
    *zsp;
extern int sstack[],		/* system stack and stack pointer */
    *ssp;

extern ZIPINT zlocs;		/* pointer to z local variables */
extern short argblk[];		/* argument block (all opcodes) */

/*  D E B U G G I N G  */

#if _DEBUG
extern char debug;			/* debug flag */
extern struct ops ins_tbl[256];
extern char opstrs[256][8];		/* opcode names */
extern int skipcnt;		/* skip n instructions */
extern int bfunc;
extern short z1, z2;		/* for setting breakpoints at ZPCn */
extern struct history_list op_hist[];
extern int last_ins;
#endif
