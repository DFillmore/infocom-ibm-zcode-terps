#include <stdio.h>
#include "zipdefs.h" 
#include "struct.h"
#include "sysdep.h"

/************************************************************************
*                                            									*
*	G L O B A L S							                                    *
*									                                             *
************************************************************************/

/*  M E M O R Y   R E L A T E D  */
unsigned int _stklen = 1024;

char Main_font[2048];
char *Alt_font = &Main_font[1024];
unsigned char far *Font;         /* font data we are currently using */
unsigned char far *FontWid;      /* and how wide is it? */
int FontNo = 0;						/* which font to use */

struct devhead cridevi;

BOOLEAN Printabt;
BOOLEAN Diskabt;

long Datend;
int Fontlen;

ZIPCHAR *Dataspace;	/* data space pointer; where code lives */ 
short Memreq;			/* total dataspace requested, in blocks */
BOOLEAN No_paging;	/* TRUE if everything loaded */
BOOLEAN Swap_to_floppy;
BOOLEAN Swapped;
BOOLEAN Isave_allowed = ZTRUE;
ZIPCHAR *Isave_buffer;        /* where the isave buffer resides */
BOOLEAN Isave_happened;
short Isave_size;             /* how big is the isave */

/*  I / O  R E L A T E D  S T U F F */
BOOLEAN Tty_busted;
unsigned char Display;        /* Video system type */

int Gamechn=-1;                  /* game file pointer */
int Savechn=-1;		            /* save file channel storage */
int Scrptfd=-1;			         /* file for scripting */
BOOLEAN File_script;				/* TRUE if scripting to a file */
int PFhandle=-1;                 /* file handle of picture file */
int Swapdrive;
BOOLEAN Floppy_script;        /* TRUE if scripting to floppy */

char *GameName =
#if defined( SHOGUN )
	"SHOGUN";
#elif defined( JOURNEY )
	"JOURNEY";
#elif defined( ZORK0 )
	"ZORK0";
#elif defined( ARTHUR )
	"ARTHUR";
#else
	"NOTHING";
#endif

char Gamfile[20],           /* name of gamefile */
     Savfile[PATHSIZ],           /* Save file name */
     Picfile[20],           /* Picture file name */
     Defdrive[3];

/*  I / O  B U F F E R S  */

char Outbuf[OBUFSIZ],		/* maximum output buffer */
	*Chrptr = Outbuf,		         /* output buffer pointer */
   Inbuf[IBUFSIZ];		   /* maximum input buffer */

/*  F L A G S  */

BOOLEAN Scripting,		   /* scripting flag */
   Scrchk,		            /* flag to check for script bit */
   Bufflg,	               /* buffering output to console */
   Vidflg = ZTRUE,	      /* outputting to screen if true */
   Scrhld;	               /* temporarily block scripting if true */
#ifdef GERMAN
    int language_ok = 1;
#endif

unsigned char Fgcolor;  /* current foreground color - actual video color */
unsigned char Bgcolor;  /* current background color - ditto */

unsigned char Def_fgcolor; /* default foreground color for this mode */
unsigned char Def_bgcolor; /* default background color for this mode */

/* the MCGA 16 RGB values for ibm zip colors */
/* Maps to the same as the EGA colors */
                              /* KEY: color,ibm_clr #,DAC reg # for mcga */    				
char Mcga_palette[48] = {
      0,0,0,            	   /* black #0 16 */
		0,0,0x2a,	            /* blue #1 17 */
		0,0x2a,0,	            /* green #2 18 */
		0,0x2a,0x2a,	         /* cyan #3 19 */
		0x2a,0,0,	            /* red #4 20 */
		0x2a,0,0x2a,	         /* magenta #5 21 */
		0x2a,0x15,0,	         /* brown #6 22 */
		0x2a,0x2a,0x2a,	      /* white_one #7 23 */
		0x15,0x15,0x15,	      /* light_grey #8 24 */
		0x15,0x15,0x3f,	      /* light_blue #9 25*/
		0x15,0x3f,0x15,	      /* light_green #10 26 */
		0x15,0x3f,0x3f,	      /* light_cyan #11 27 */
		0x3f,0x15,0x15,	      /* light_red #12 28 */
		0x3f,0x15,0x3f,	      /* light_magenta #13 29 */
		0x3f,0x3f,0x15,         /* yellow #14 30 */
		0x3f,0x3f,0x3f};        /* white #15 31*/

char Ega_palette[17] = {      /* init palette and border for EGA */
   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
   0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
   0x00 };

char Cga_to_zip[] = {       /* just black and white for CGA */
   ZIP_BLACK, ZIP_WHITE, ZIP_BLACK, ZIP_WHITE };

char Zip_to_cga[] = {        /* map ZIP colors to CGA foreground */
   0x00,    /* ZIP_BLACK */
   0x00,    /* ZIP_RED */
   0x00,    /* ZIP_GREEN */
   0x00,    /* ZIP_YELLOW */
   0x01,    /* ZIP_BLUE */
   0x01,    /* ZIP_MAGENTA */
   0x01,    /* ZIP_CYAN */
   0x01,		/* ZIP_WHITE */
	0x00,		/* ZIP_GREY */
	0x01		/* ZIP_BROWN */
	};


/* Mcga colors run from 16-31 */
char Mcga_to_zip[] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   ZIP_BLACK, ZIP_BLUE, ZIP_GREEN, ZIP_CYAN, ZIP_RED, ZIP_MAGENTA, ZIP_BROWN,
   ZIP_WHITE, ZIP_GREY, ZIP_BLUE, ZIP_GREEN, ZIP_CYAN, ZIP_RED, ZIP_MAGENTA, ZIP_YELLOW, ZIP_WHITE };

char Zip_to_mcga[] = {        /* map ZIP colors to MCGA */
   16,      /* ZIP_BLACK */    
   20,      /* ZIP_RED */
   18,      /* ZIP_GREEN */
   30,      /* ZIP_YELLOW */
   25,      /* ZIP_BLUE */
   21,      /* ZIP_MAGENTA */
   19,      /* ZIP_CYAN */
   31,		/* ZIP_WHITE */
	24,		/* ZIP_GREY */
	22			/* ZIP_BROWN */
	};

char Ega_to_zip[] = {
   ZIP_BLACK, ZIP_BLUE, ZIP_GREEN, ZIP_CYAN, ZIP_RED, ZIP_MAGENTA, ZIP_BROWN,
   ZIP_WHITE, ZIP_GREY, ZIP_BLUE, ZIP_GREEN, ZIP_CYAN, ZIP_RED, ZIP_MAGENTA, ZIP_YELLOW, ZIP_WHITE };

char Zip_to_ega[] = {        /* map ZIP colors to EGA */
   0,       /* ZIP_BLACK */
   4,       /* ZIP_RED */
   2,       /* ZIP_GREEN */
   14,      /* ZIP_YELLOW */
   1,       /* ZIP_BLUE */
   5,       /* ZIP_MAGENTA */
   3,       /* ZIP_CYAN */
   15,		/* ZIP_WHITE */
	9,			/* ZIP_GREY */
	7			/* ZIP_BROWN */
	};


char *Ibm_to_zip;          /* 16 char array to map IBM color to ZIP color */
char *Zip_to_ibm;          /* 8 char array to map ZIP color to IBM color */

char far *vb_ptr;          /* video buffer pointer */

#ifdef DO_TANDY
int Tandy_palette[4];      /* 4 color tandy palette */
#endif

BOOLEAN Mouseflg;                /* mouse flag  0 not installed */
int Mouse_event_count;           /* number of events in mouse queue */
int Mouse_queue_start=0;	      /* points to next place to read from */
int Mouse_event[MOUSE_EVENT_LENGTH];
int Mouse_x[MOUSE_EVENT_LENGTH];
int Mouse_y[MOUSE_EVENT_LENGTH];
int Mouse_buttons;
int Mouse_cur_x;
int Mouse_cur_y;
int Mouse_frob[3];
ZIPBYT Mouse_window = 1;           /* which window constrains mouse */

/*  S C R E E N   S T U F F   */
int Linecnt;			      /* line count for MORE */
int Scrbtm;                /* bottom line of the screen */
BOOLEAN Dosprint;          /* script using bios normally */
BOOLEAN Mouse_on;		      /* don't use mouse unless asked */
char Oldmode;
char Mode_bits;            /* supported display modes */
int Scrx, Scry;            /* Current cursor x,y position */
int CharX;                 /* Running offset of printing line */
int Scrfont=1;             /* Current screen font */
int Win_ypos, Win_xpos;    /* Current window y,x position */
int Win_ysize, Win_xsize;  /* Current window's height, width */
int Win_right;             /* right hand column, inclusive, of current window */
int Win_lmarg, Win_rmarg;  /* Current window's left/right margins */
int Win_hlmode;            /* Current highlight mode */
int Win_attributes;        /* Attributes currently in force */
int Screen;                /* Current screen number */
int Crcnt;                 /* <CR> count */

/* for OPDIROUT */
ZIPINT Table,	            /* pointer to table */
   Tblptr,                 /* pointer to place for next chr in table */
   Tblchars,	            /* number of chrs output to table */
   FTblwidth;               /* width of (formatted) table */

BOOLEAN Tblout;            /* Doing normal table output? */
BOOLEAN FTblout;           /* Doing formatted table output? */

int Lines;                 /* lines per page */

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

unsigned char *character_set =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ \n0123456789.,!?_#'\"/\\-:()";


char read_terminator = 0;

unsigned long FOFF8;
unsigned long SOFF8;

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

short last_block_size;
short game_blocks;

short zpc1,			/* z program counter block number */
    zpc2,			/* z program counter byte offset */
    zblk,			/* roving zpointer (bsplit/getbyt/putstr), */
    zoff,			/*   usually different than zpc */
    curblk = -1,		/* current block (same as last zpc1) */
    curpag = -1;		/* last page gotten (from getpag) */

ZIPCHAR *curblkloc;		/* pointer to curblk block */
ZIPCHAR *curpagloc;		/* pointer to curpag block */

struct blkdesc 
    far *pagedesc;		/* one descriptor for each virtual page */
int far *pagemap;	      /* one mapping for each virtual page */
int mru;

/*  Z - S T A C K   A N D   S Y S T E M   S T A C K  */

ZIPINT zstack[LSTACK],		/* z stack and stack pointer */
    *zsp;

ZIPINT zlocs;			      /* pointer to z local variables */
ZIPBYT zargct;
short argblk[MAXARGS];		/* argument block (all opcodes) */

/* these are some maximums that depend on video data */
int Max_screen_width;
int Max_screen_height;
int Zip_font_height = FONT_HEIGHT;           /* normal font for video mode */
int Zip_font_width = FONT_WIDTH;
int Font_height = FONT_HEIGHT;               /* current font height/width */
int Font_width = FONT_WIDTH;

unsigned short *last_de;		/* last dir. ent. that had a palette */

/* some system stuff */
int Dos_version = 0;					/* DOS version */
int Cur_drive;							/* Drive \game file is on */
int Nfloppies = 0;					/* Number of floppy drives on system */
char Defpath[PATHSIZ];				/* default path for save file name */

char pic_errflg = ZFALSE;
char far *picture_buff;

BOOLEAN Mouse_shows;
BOOLEAN Cursor_on;                        /* TRUE if cursor is actually showing */
BOOLEAN Do_cursor = ZTRUE;                /* FALSE if no fooling with it */

struct WINDOW wattrib[8];

/******* Some globals for the picture stuff ******/
void far *Hash_buff;         /* 12K LZ hash landing area */      
void far *Pic_buff;          /* available buffer pre-allocated */
unsigned int Pic_buff_size;  /* size in paragraphs of Pic_buff */
unsigned char Display;       /* Display mode (1, 2, or 4) */     
