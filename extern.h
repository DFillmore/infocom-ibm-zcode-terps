/************************************************************************
*									*
*	E X T E R N S							*
*									*
************************************************************************/

/*  M E M O R Y   R E L A T E D  */

extern char *alt_font;
extern char main_font[];

extern ZIPCHAR *dataspace;	/* data space pointer; where code lives */ 
extern short memreq;		/* total dataspace requested, in blocks */
extern int no_paging;
extern int swap_to_floppy;
extern int swapdrive;
extern int swapped;

/*  F I L E   C H A N N E L S   A N D   D E S C R I P T O R S  */

extern int gamechn, savechn;		/* file channel storage */
extern int scrptfd;			/* channel for scripting */
					/* -1 means printer */
extern int floppy_script;

extern long datend;
extern int fontlen;

extern int printabt;
extern int diskabt;
extern struct devhead cridevi;

extern char *gamfile,
     rgamfile[],
     savfile[],
     defdrive[];

#ifdef XZIP
extern int isave_allowed;		/* default to yes */
extern ZIPCHAR *isave_buffer;		/* pointer to memory for it */
extern int isave_happened;		/* if it ever happened */
extern short isave_size;
#endif

/*  I / O  B U F F E R S  */

extern char *chrptr, *endbuf,		/* output buffer pointers */
#ifdef ZIP
     *p_chrptr, *p_endbuf,	/* pipe output buffer pointers */
     p_outbuf[],		/* pipe output buffer */ 
#endif
     outbuf[],		/* maximum output buffer */
     inbuf[];		/* maximum input buffer */
extern int chars_on_line;	/* number of characters since last CR
				   in screen 0 */

/*  F L A G S  */

extern char scripting,		/* scripting flag */
     scrchk,			/* flag to check for script bit */
     slflg,			/* status line in place (EZIP) */
#ifdef ZIP
     scroll,			/* windowed scrolling */
#endif
     screen,			/* current screen for output */
#ifdef ZIP
     splitable,			/* ablity to support split screen */
#endif
     spltflg;			/* screen split flag -- top line of scr0 */
#ifdef ZIP
extern char toplin,
#endif

#ifdef EXZIP
extern char intwrd[];		/* machine id, interpreter version */
extern char bufflg, prevbuf;	/* buffering output to console */
extern char slpp,               /* length of screen in rows */
     topscr,
     vidflg,			/* is output directed to screen? */
     scrhld;			/* is scripting temporarily off? */
extern int graphics;		/* are we in graphics mode or alpha mode? */
#endif
#ifdef XZIP
#ifdef GERMAN
extern int language_ok;
#endif
extern char fontsize;
extern char bgcolor;
extern char fgcolor;
extern char zip_to_ibm_color[];
extern char ibm_to_zip_color[];

/* MOUSE VARIABLES */
extern int mouseflg; /* 0 if no mouse */
extern int mouse_x[];
extern int mouse_y[];
extern int mouse_event[];
extern int mouse_event_count;
extern int mouse_queue_start;
extern int mouse_buttons;
extern int mouse_cur_x;
extern int mouse_cur_y;
extern int mouse_frob[];

#endif

/*  T T Y   */

extern int ttyfd,		/* for setting up terminal i/o */
    ttysav;			/* storage of a startup tty condition */
extern int tandy;
extern int ega;
extern int tty_busted;
extern int winlen,		/* window length */
    linecnt;			/* line count for MORE */
extern int RM,LM;		/* margins */
extern int scrwid;		/* actual screen width */
extern int curscrwid;		/* current screen width */
extern int dosprint;		/* if set, means print using dos calls */
extern int docolor;		/* -1 means no, 1 means yes, 0 means don't
  care.  color is currently not supported anyway */
extern int mouse_on;		/* if true, user wants to use mouse */
extern char oldmode;		/* old screen mode, restore on quit */
extern char scrmode;		/* from INT 10 */
extern char normattr;		/* normal character attribute */
extern char curattr;		/* current attribute */
extern char highlight_bits;	/* what we can do */
extern char current_highlight;	/* what we're currently doing */
extern char scr0x,scr0y,scr1x,scr1y,scrx,scry;
extern int scr0font, scr1font, scrfont;
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
    main_vocabulary;		/* pointer to main vocab table */

char read_terminator;		/* character that terminated read */

int not_terminator();		/* function to see if char in tchars */

extern unsigned char character_set[];

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
     irbrks[];			/* ^^ initial read break chars */

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

extern short last_block_size;	/* number of BYTES in last block of game */
extern short game_blocks;	/* number of blocks in game */

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

extern ZIPINT zlocs;		/* pointer to z local variables */
extern char zargct;		/* number of args this routine called with */
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

