#ifndef ZIPDINC
#include "zipdefs.h"
#endif

/************************************************************************
*																								*
*	S T R U C T U R E S																	*
*																								*
************************************************************************/

struct devhead {
	long	dhpnext;
	int	dhattrs;
	int	dhstrat;
	int	dhint;
	char	dhname[8]; };

typedef struct blkdesc {
   short next;			/* next descriptor index */
   short prev;			/* previous descriptor index  */
   ZIPCHAR *loc;		/* page pointer */
   short vpage;		/* page number */
   }blkdesc;

struct WINDOW {
   int ypos, xpos;         /* position of window */
   int ysize, xsize;       /* height, width of window */
   int ycurpos, xcurpos;   /* y,x of cursor, relative to window */
   int lmarg, rmarg;       /* left/right margin */
   int crintfcn;           /* <CR> function address */
   int crintctr;           /* <CR> function counter */
   int hlmode;             /* highlight mode */
   int fgcolor;            /* foreground color */
   int zipfg;              /* ZIP foreground color */
   int bgcolor;            /* background color */
   int zipbg;              /* ZIP background color */
   int fontid;             /* which font */
   struct {
      char height;         /* font height */
      char width;          /* font width */
   } fontsize;             /* complete font size */
   int attributes;         /* wrap/scroll/script/buffer */
   int linecnt;
};

/* Picture file stuff */
/* this is how the local directory entry looks in the file */
struct PF_LOCAL_DIR {
   int   id;                  /* actual ID */
   int   width;               /* width of picture */
   int   height;              /* height of picture */
   int   flags;               /* various interesting flags (see LDF_) */
   unsigned char dataptr[3];  /* 3 byte offset to file data */
   unsigned char palptr[3];   /* 3 byte offset to palette for file */
};

/* these two structures, one with palptr and one without, reflect the
 two different kinds of structures possible in picture file local directory
 entry. */
struct PF_LD_PAL {
   int   id;                  /* actual ID */
   int   width;               /* width of picture */
   int   height;              /* height of picture */
   int   flags;               /* various interesting flags (see LDF_) */
   unsigned char dataptr[3];  /* 3 byte offset to file data */
   unsigned char palptr[3];   /* 3 byte offset to palette for file */
};
struct PF_LD_XPAL {
   int   id;                  /* actual ID */
   int   width;               /* width of picture */
   int   height;              /* height of picture */
   int   flags;               /* various interesting flags (see LDF_) */
   unsigned char dataptr[3];  /* 3 byte offset to file data */
   unsigned char pad;         /* not much else */
};

/* this is after I muck with it a bit */
struct LOCAL_DIR {
   int   id;                  /* actual ID */
   int   width;               /* width of picture */
   int   height;              /* height of picture */
   int   flags;               /* various interesting flags (see LDF_) */
   long  offset;              /* where is the data */
   long  palette;             /* where is the palette (possibly zero) */
   unsigned char tr_color;    /* transparent color */
   };

/* Picture File header data */
struct PF_HEADER {
   unsigned char id;          /* File ID */
   unsigned char flags;       /* interesting global flags */
   int huffman;               /* inert spot, now that we do LZ */
   int ld_count;              /* how many id's in local directory */
   int gblptr;						/* where is global directory */
   unsigned char ld_size;     /* local directory size */
   int checksum;              /* picture file checksum */
   int hdrvers;               /* kinda meaningless */
   int version;               /* picture file version */
};
