/* IBM graphics for XZIP */

#include "stdio.h"

#define BYTPROW 80
#define COLPBYT 4

#define ROWOFF(row) (row * BYTPROW + ((row &1) ? 0x2000 - BYTPROW : 0))
#define COLOFF(col) (col / COLPBYT)


#define NFONTS 5	/* maximum number of picture files open at once */

char *pfname = "FONT2.DAT";
int curfont = 0;		/* number of current font */

#define FNUMP (pfname+4)	/* points at 2 in FONT2.DAT */
        
struct pfilhdr {
    int gcount;		/* number of definitions in file */
    int goff;		/* word offset to initial picture/character */
    char gwidth;	/* zero if picture file... */
    char gheight;	/* ... else sizes of characters */
    int gflags;		/* bit 0 is Front Screen bit */
    int grest[12];	/* reserved */
};

struct pictfile {
    FILE *pffp;
    int pfnum;
    struct pfilhdr pfhdr;
};

struct pictfile pfiles[NFONTS], *curpfile;

/* offsets in picture file  */
    
#define POFFSIZE sizeof(struct pfilhdr)
#define POFFCHAR POFFSIZE

/*   might want this if caching
  
#define BUFSIZE 512

struct pbufs {
    struct picobj pbpic;	/-* picture using this buffer *-/
    char pbbuf[BUFSIZE];	/-* background data/picture *-/
};

struct picobj {
    int picnum;
    int picwidth;
    int picheight;
    struct pbufs picbuf;
};

getpict(num, pfobj) int num; struct pictfile pfobj; {
}
*/
/* display a character or picture from the current font */

#define ISPICT(pfp) (pfp->pfhdr.gwidth == 0)

disppict(num, x, y) int num, x, y; {
    struct pictfile *pfilep;
    FILE *fp;
    long seekptr;
    int numrel;
    int wtemp, xtemp, i, j, k, byte;
    int height, width, wbytes, w;
    pfilep = curpfile;
    fp = pfilep->pffp;
    numrel = num - pfilep->pfhdr.goff;	/* pict num rel to origin */
    if (ISPICT(pfilep)) {
	seekptr = (long)POFFSIZE;
	fseek(fp, seekptr, SEEK_SET);
	for (i = 0; i != numrel; i++ ) {
	    width = pfgetw(fp);
	    height = pfgetw(fp);
	    wbytes = ((width + 15) >> 4) << 1; /* bytes/row, wd boundary */
	    seekptr += wbytes * height;
		    /* accumulate sizes of earlier pics */
	    }
	width = pfgetw(fp);
	height = pfgetw(fp);
	wbytes = ((width + 15) >> 4) << 1; /* bytes per row, word boundary */
	seekptr += (pfilep->pfhdr.gcount << 4);
	    /* skip size(12) & name(4) tables: 16 bytes * num of pics */
	fseek(fp, seekptr, SEEK_SET);	/* start of chr */
	}
    else {
	width = pfilep->pfhdr.gwidth;
	height = pfilep->pfhdr.gheight;
	wbytes = (width + 7) >> 3;	/* round up to bytes per chr */
	seekptr = (long)(wbytes * height) * (long)numrel + (long)POFFSIZE;
	fseek(fp, seekptr, SEEK_SET);	/* start of chr */
	}
    for (i = 0; i < height; i++, y++) {
	xtemp = x;
	for (j = wbytes, w = width; w > 0; j--, w-=8) {
	    byte = fgetc(fp);
	    k = 8;
	    do {
		wdot(y, xtemp++, (byte & 128) ? 1 : 0);
		byte <<= 1;
		} while (--k);
	    }
	if (j) fgetc(fp);	/* possible word-alignment byte */
	}
    
}

/* load in a new font if necessary, set the current font to the given
   font */

font(n) int n; {
    struct pictfile *pfilep;
    FILE *fp;
    if (curfont == n) return(n);
    curfont = 0;			/* default */
    pfilep = &pfiles[n];
    if (pfilep->pfnum == n) return (n);	/* already loaded */
    *FNUMP = '0' + n;
    if ((fp = fopen(pfname, "r")) == NULL) return(0);	/* err chk */
    pfilep->pffp = fp;			/* success */
    pfilep->pfnum = n;			/* show it is loaded */
    pfilep->pfhdr.gcount = pfgetw(fp);	/* read header info */
    pfilep->pfhdr.goff = pfgetw(fp);
    pfilep->pfhdr.gwidth = fgetc(fp);
    pfilep->pfhdr.gheight = fgetc(fp);
    pfilep->pfhdr.gflags = pfgetw(fp);
    curpfile = pfilep;
    return(curfont = n);
}

/* get a word from a FILE, high byte first */

pfgetw(fp) FILE *fp; {
    int temp;
    temp = fgetc(fp);			/* high byte first */
    return((fgetc(fp) & 255) + (temp << 8));
}

/* fontptr(f) returns the struct pictfile pointer to the font header */

struct pictfile *
fontptr(f) int f; {
    struct pictfile *pfilep;
    pfilep = &pfiles[f];
    if (pfilep->pfnum != f) {
	if(!font(f)) return(0);	/* load if necessary */
	pfilep = &pfiles[f];
	}
    return(pfilep);
}

/* fontsize(font) returns gsize value
   fontoff(font) returns goff value of font */

fontsize(f) int f; {
    struct pictfile *pfilep;
    if (pfilep = fontptr(f)) return(pfilep->pfhdr.gcount);
    return(0);
}
    
fontoff(f) int f; {
    struct pictfile *pfilep;
    if (pfilep = fontptr(f)) return(pfilep->pfhdr.goff);
    return(0);
}

/* initialize the font file headers and the current font */

grinit() {
    struct pictfile *pfp;
    int f;
    for (f = 0; f < NFONTS; f++) {
	pfp = &pfiles[f];
	pfp->pffp = 0;
	pfp->pfnum = 0;
	}
    curfont = 0;
}
    