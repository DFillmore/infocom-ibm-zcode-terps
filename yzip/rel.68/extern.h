/************************************************************************
*                                            									*
*	G L O B A L S							                                    *
*									                                             *
************************************************************************/

/*  M E M O R Y   R E L A T E D  */
extern unsigned int _stklen;

extern char Main_font[2048];
extern char *Alt_font;
extern unsigned char far *Font;        /* current font data */
extern unsigned char far *FontWid;     /* and how wide is it? */
extern int FontNo;							/* which font to use */

extern BOOLEAN Printabt;
extern BOOLEAN Diskabt;
extern BOOLEAN No_abort;		/* if true, just do an abort without asking */

extern long Datend;
extern int Fontlen;

extern ZIPCHAR *Dataspace;	/* data space pointer; where code lives */ 
extern short Memreq;			/* total dataspace requested, in blocks */
extern BOOLEAN No_paging;	/* TRUE if everything loaded */
extern BOOLEAN Swap_to_floppy;
extern BOOLEAN Swapped;
extern BOOLEAN Isave_allowed;
extern ZIPCHAR *Isave_buffer;        /* where the isave buffer resides */
extern BOOLEAN Isave_happened;
extern short Isave_size;             /* how big is the isave */

/*  I / O  R E L A T E D  S T U F F */
extern BOOLEAN Tty_busted;
extern unsigned char Display;

extern int Gamechn;                  /* game file pointer */
extern int Savechn;		            /* save file channel storage */
extern int Scrptfd;			         /* file for scripting */
extern BOOLEAN File_script;				/* TRUE if scripting to a file */
extern int PFhandle;                 /* file handle of picture file */
extern int Swapdrive;
extern BOOLEAN Floppy_script;        /* TRUE if scripting to floppy */
extern char *GameName;				/* what game is this */

extern char Gamfile[PATHSIZ],          /* name of gamefile */
     Savfile[PATHSIZ],           /* Save file name */
     Picfile[PATHSIZ],           /* Picture file name */
     Defdrive[3];

/*  I / O  B U F F E R S  */

extern char *Chrptr,		/* output buffer pointer */
     Outbuf[],		/* maximum output buffer */
     Inbuf[];		   /* maximum input buffer */

/*  F L A G S  */

extern BOOLEAN Scripting,		   /* scripting flag */
   Scrchk,		            /* flag to check for script bit */
   Bufflg,	               /* buffering output to console */
   Vidflg,        	      /* outputting to screen if 1 */
   Scrhld;	               /* temporarily block scripting if 1 */
#ifdef GERMAN
extern int language_ok;
#endif

extern unsigned char Fgcolor; /* current foreground color - actual video color */
extern unsigned char Bgcolor; /* current background color - ditto */

extern unsigned char Def_fgcolor; /* default foreground color for this mode */
extern unsigned char Def_bgcolor; /* default background color for this mode */

extern char Mcga_palette[48];
extern char Ega_palette[17];      /* init palette and border for EGA */
extern char Cga_to_zip[];       /* map CGA foreground color to zip color */
extern char Mcga_to_zip[];
extern char Ega_to_zip[];
extern char Zip_to_cga[];        /* map ZIP colors to CGA foreground */
extern char Zip_to_ega[];        /* map ZIP colors to EGA */
extern char Zip_to_mcga[];       /* map ZIP colors to EGA */
extern char *Ibm_to_zip;         /* 16 char array to map IBM color to ZIP color */
extern char *Zip_to_ibm;         /* 8 char array to map ZIP color to IBM color */

#ifdef DO_TANDY
extern int Tandy_palette[4];      /* 4 color tandy palette */
#endif
    				
extern BOOLEAN Mouseflg;                /* mouse flag  0 not installed */
extern int Mouse_event_count;           /* number of events in mouse queue */
extern int Mouse_queue_start;	      /* points to next place to read from */

#ifdef MOUSE_EVENT_LENGTH
extern int Mouse_event[MOUSE_EVENT_LENGTH];
extern int Mouse_x[MOUSE_EVENT_LENGTH];
extern int Mouse_y[MOUSE_EVENT_LENGTH];
#endif

extern int Mouse_buttons;
extern int Mouse_cur_x;
extern int Mouse_cur_y;
extern int Mouse_frob[3];
extern ZIPBYT Mouse_window;           /* which window constrains mouse */

/*  S C R E E N   S T U F F   */
extern int Linecnt;			      /* line count for MORE */
extern int Scrbtm;                /* bottom line of the screen */
extern BOOLEAN Dosprint;          /* script using bios normally */
extern BOOLEAN Mouse_on;		      /* don't use mouse unless asked */
extern char Oldmode;
extern char Mode_bits;
extern int Scrx, Scry;           /* Current cursor x,y position */
extern int CharX;                 /* Running offset of printing line */
extern int Scrfont;               /* Current screen font */
extern int Win_ypos, Win_xpos;    /* Current window y,x position */
extern int Win_ysize, Win_xsize;  /* Current window's height, width */
extern int Win_lmarg, Win_rmarg;  /* Current window's left/right margins */
extern int Win_right;             /* right hand column, inclusive, of current window */
extern int Win_hlmode;            /* Current highlight mode */
extern int Win_attributes;        /* Attributes currently in force */
extern int Gpix_on_line;		/* used for mcga mode 0x13 */
extern int Screen;                /* Current screen number */
extern int Crcnt;                 /* <CR> count */

extern int Lines;                      /* lines per page */

/*  T A B L E   P O I N T E R S  */

extern ZIPINT timemd,			/* time/score mode */
    zorkid,			/* game id */
    endlod,			/* endlod pointer */
    voctab,			/* vocabulary table ptr */
    objtab,			/* object table ptr */
    glotab,			/* global table ptr */
    wrdtab,			/* word table ptr */
    wrdoff,			/* offset to current wrdtab */
    purbot,			/* pure load ptr */
    main_vocabulary;		/* primary vocabulary table...*/

extern unsigned char *character_set;
extern char read_terminator;

/* for OPDIROUT */
extern ZIPINT Table,	            /* pointer to table */
   Tblptr,                 /* pointer to place for next chr in table */
   Tblchars,	            /* number of chrs output to table */
   FTblwidth;               /* width of (formatted) table */

extern BOOLEAN Tblout;            /* Doing normal table output? */
extern BOOLEAN FTblout;           /* Doing formatted table output? */

extern unsigned long FOFF8;
extern unsigned long SOFF8;

/*  O P R E A D   G L O B A L S  */ 

extern ZIPCHAR *curword;		/* ascii input word pointer */
extern ZIPCHAR *rdbos;			/* beginning of read string (table 1) */
extern ZIPCHAR *nxttok;		/* pointer to next input word */

extern char *lastbrk,			/* pointer to last ascii break char */
     *esibrks,			/* end of self-inserting break chars */
     rbrks[32],			/* string of read break chars */
     irbrks[];
     
extern short rdwstr[CHRS_PER_ZWORD/3],	/* vocabulary word repository during lookup */
      rdeos,			/* read end of string (offset) */
      curoff;			/* current offset in ascii input buffer */

/*  R A N D O M  */

extern int rseed;			/* seed for random numbers */
extern int rcycle;			/* holds upper bound for non-random cycles */
extern int rconst;			/* holds 1 to <rcycle> */

/*  P A G I N G  */

extern short last_block_size;
extern short game_blocks;

extern short zpc1,			/* z program counter block number */
    zpc2,			/* z program counter byte offset */
    zblk,			/* roving zpointer (bsplit/getbyt/putstr), */
    zoff,			/*   usually different than zpc */
    curblk,		/* current block (same as last zpc1) */
    curpag;		/* last page gotten (from getpag) */

extern ZIPCHAR *curblkloc;		/* pointer to curblk block */
extern ZIPCHAR *curpagloc;		/* pointer to curpag block */

extern struct blkdesc far *pagedesc;	/* one descriptor for each virtual page */
extern int far *pagemap;	                     /* one mapping for each virtual page */
extern int mru;

/*  Z - S T A C K   A N D   S Y S T E M   S T A C K  */

extern ZIPINT zstack[LSTACK],		/* z stack and stack pointer */
    *zsp;

extern ZIPINT zlocs;			      /* pointer to z local variables */
extern ZIPBYT zargct;
extern short argblk[MAXARGS];		/* argument block (all opcodes) */

/* these are some maximums that depend on video data */
extern int Max_screen_width;
extern int Max_screen_height;
extern int Zip_font_height;          /* normal font for video mode */
extern int Zip_font_width;
extern int Font_height;               /* current font height/width */
extern int Font_width;

extern unsigned short *last_de;		/* last dir. ent. that had a palette */

/* some system stuff */
extern int Dos_version;					/* DOS version */
extern int Cur_drive;							/* Drive game file is on */
extern int Nfloppies;					/* Number of floppy drives on system */
extern char Defpath[PATHSIZ];				/* default path for save file name */

extern char pic_errflg;
extern char far *picture_buff;

extern char far *vb_ptr;          /* video buffer pointer */

extern unsigned char Font0, Font0Wid;
extern unsigned char Font1, Font1Wid;
extern unsigned char Font2, Font2Wid;
extern unsigned char Font3, Font3Wid;

extern BOOLEAN Mouse_shows;
extern BOOLEAN Cursor_on;                        /* TRUE if cursor is actually showing */
extern BOOLEAN Do_cursor;                /* FALSE if no fooling with it */

extern struct WINDOW wattrib[8];

/******* Some globals for the picture stuff ******/
extern void far *Hash_buff;         /* 12K LZ hash landing area */      
extern void far *Pic_buff;          /* available buffer pre-allocated */
extern unsigned int Pic_buff_size;  /* size in paragraphs of Pic_buff */
extern unsigned char Display;       /* Display mode (1, 2, or 4) */     

#include "proto.h"


