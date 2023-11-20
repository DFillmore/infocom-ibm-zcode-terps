/************************************************************************
*									*
*									*
*		TITLE:	Z-LANGUAGE INTERPRETIVE PROGRAM			*
*									*
*			      FOR UNIX IN C				*
*									*
*		 Copyright Infocom, Inc. (c)(p), 1985			*
*									*
*			Written by Paul H. Gross			*
*			   on 5-October-1985				*
*									*
*									*
************************************************************************/


#include "zipdefs.h"
#include "struct.h"
#include "extern.h"

#ifdef IBMPC
#include "fcntl.h"
#include "stdlib.h"
#endif

/* EZIP should include ezipdefs.h */

/*  C O M M O N   R O U T I N E S   */

/* Declared here instead of in the header of every routine that uses them */

ZIPINT GTAWRD(), GTVWRD();
char lc();


/************************************************************************
*									*
*	M A I N   P R O G R A M   A N D   I N I T S			*
*									*
************************************************************************/

main(argc,argv)  
int argc; char **argv; 
{
    char *datname, *init();
    void main_loop();

#ifdef IBMPC
    _fmode = O_BINARY;	/* set the default open mode to NOT TEXT */
#endif

    md_setup();		/* do machine dependent inits */

    if (datname = init(argc,argv)) THEN	/* get command line stuff */
      gamfile = datname;

    sysini();		/* do system initialization */
    memini();		/* setup memory, preload, paging, and tty */
    zipini();		/* setup zip header info and table pointers */
    do 
     main_loop();
     while (quit == 0);
    z_exit();
}

void main_loop() {
    do {		/* main loop */
#if _DEBUG
      if (debug) THEN debugger();
      else
#endif
	nxtins();
    }
    while (end_loop == 0);	/* (not optimal speedwise, but structured) */
}

char *init(argc,argv) 
int argc;
char **argv;
{  /* Init processes command line parameters, figures the dat file name to use,
      and sets up the debugger if requested */

    char *prog, *s, *datfile = 0, *dprog = "eziptest", *ext = ".zip";
    short i, locmem = 0;
#if _DEBUG
    char str[10], *tstr;
    int op;
    FILE *opchnfp; 
#endif

    prog = argv[0];
    while (--argc) {
#ifdef IBMPC
      if ((*++argv)[0] == '/') {
#else
      if ((*++argv)[0] == '-') {
#endif
	for (s = &((*argv)[1]); *s; s++) {
	  switch (lc(*s)) {
#if _DEBUG
	    case 'd': {			/* turn on debugger */
		debug = ON | VERBOSE; 
		for (i = 0; i <= 255; i++) {
		  ins_tbl[i].brkflg = 0;
		  ins_tbl[i].opstr = opstrs[0];
		  }
		opchnfp = fopen("ops.dat", "r");
		while (fscanf(opchnfp, "%s %d", str, &op) != EOF) {
		  i = 0;
		  tstr = str;
		  while (*tstr) 
		    opstrs[op][i++] = *tstr++;
		  ins_tbl[op].opstr = opstrs[op];
		  }
		fclose(opchnfp);
		break;
	   	} 
#endif
	    case 'c': {
		docolor = 1;
		break; }
	    case 'm': {
		docolor = -1;
		break; }
	    case 'x': {
		graphics = 1;
		break; }
	    case 'a': {
		graphics = -1;
		break; }
	    case 'k': {			/* max dataspace request, in K */
		s++;
		while (*s) {
		  locmem *= 10;		/* make a decimal number */
		  locmem += *s - '0';	/* asciify string */
		  s++;			/* advance pointer */
		  }
		s--;			/* back up one */
		break;
		}
	    case 'g': {			/* game file to use */
		datfile = (s+1);
		while (*(s+1)) s++;	/* skip rest of arg */
		break;
		}
	    default : printf("\nUnknown switch: %c\n", lc(*s)); break;
	    }					/* end of switch */
	  }					/* end of for loop */
	}					/* end of if loop */
      }						/* end of while loop */
    if (locmem) THEN
      memreq = locmem * KTOBLKS;	/* convert k to blocks */
    else memreq = 0;			/* otherwise use default */

    if (datfile == 0) THEN {
      s = dprog;			/* get program name */
      i = 0;		/* temporary default,make it easy to get eziptest*/
      while (*s) 
	gamfbuf[i++] = *s++;
      s = ext;
      while (*s)
	gamfbuf[i++] = *s++;		/* add on ".dat" */
      datfile = gamfbuf;
      }
    return(datfile);
}

sysini()
{   /*  Sysini opens the data file, saves away the name as the default save
	name, and determines total available memory.
    */
    char *d, *s, *ext = ".sav";
    int memavail;
    long length, filen();
    
    if ((gamechn = open(gamfile, RDONLY)) < 0) THEN {
      printf("Failed to open game file -- %s", gamfile);
      fatal("Sysini");
      }
    s = gamfile;
    d = savfile;
    while (*s != PERIOD)
      *d++ = *s++;			/* copy game file name */
    s = ext;
    while (*d++ = *s++);		/* add .SAV extension */

    length = filen(gamechn);		/* mem req size = file length */
    if (memreq == 0) memreq = (length + BLKSIZ - 1) >> CVTBLK; /* round up */
    
    memavail = md_msize(memreq);	/* blocks available(desired) */
    memavail &= ~BIT16;			/* make sure it's unsigned */

    if (memreq > memavail) THEN		/* limit memreq to memavail */
      memreq = memavail;
    if (!memreq) THEN			/* default, request memavail */
      memreq = memavail;
}

static char temp_buffer[MIN_HEADER];	/* temp space for getting game size */

memini()
{ /*	This routine compares memreq with ENDLOD and PLENTH.  It
	determines how much dataspace to allocate, and does so.  It determines
	how much data to preload, and does so.  It also initializes paging.
  */
    ZIPINT maxlod;
    short i;
    ZIPCHAR *md_alloc();

/*  Read the first block into a temporary buffer.  We temporarily set
    dataspace to point to this buffer, so that getpre() and the GTV macros
    work.  */

    dataspace = temp_buffer;
    getsblk(0,MIN_HEADER,dataspace);		/* get block 0 */

    md_init_screen();		/* determine color and such...*/
    endbuf = outbuf + scrwid;	/* determine output buffer width */
#ifdef EZIP
    if (scrwid < 60) THEN
      fatal("Screen too narrow.");
#endif
    p_endbuf = p_outbuf + PIPEWIDTH;	/* and pipe width */

    endlod = GTVWRD(PENDLD);		/* get endlod pointer */
    endlod = (endlod + BLKSIZ-1) >> CVTBLK; /* round & convert to blocks */

    maxlod = GTVWRD(PLENTH);		/* length of program, in words */
#ifdef ZIP
    maxlod += BLKSIZ/2 - 1;		/* round up to next block */
    maxlod >>= CVTBLK-1;		/* convert words to blocks */
#endif
#ifdef EXZIP
    maxlod += BLKSIZ/4 - 1;		/* round up to next block */
    maxlod >>= CVTBLK-2;		/* convert quads to blocks */
#endif

/*  Note that our paging scheme normally requires a minimum of 2 pages in 
    the chain, one for the current code page and a second for roving pointers.
    In the freak case where only one page is not preloaded, however, the
    "chain" may contain only one page too.  When all pages are preloaded, 
    paging is never called and no chain at all is required.  Thus an array
    of MAXBLKS paging structures is the most ever needed.
*/
    if (memreq < endlod + 2) THEN
      fatal("Insufficient memory for preload");

    if (memreq >= maxlod) THEN {	/* mucho memory, take advantage */
      endlod = maxlod;		/* hack endlod to force total preload */
      memreq = maxlod;		/* reduce memreq to max needed */
      }

    if ((dataspace = md_alloc((long)memreq * BLKSIZ))
	== (ZIPCHAR *)NULL) THEN {
      printf("Unable to allocate %d", memreq, "blocks");
      fatal("Memory allocation error");
      }
    getpre(0, endlod);			/* read in preload data */

/*  Currently, an array of blkdescs and a pagemap are declared statically
    [0..255].  Should allocate space dynamically for [endlod..memreq-1] only 
    (number of physical buffers), and a pagemap array for [0..maxlod-1] only
    (number of actual pages).

    IDEA:  call getpre(endlod, memreq) to "prime" the page buffers, and 
    mark each pagedesc and pagemap appropriately.
*/

    if (endlod < maxlod) THEN {		/* if total preload, just skip */

      for (i = 0; i < MAXBLKS; i++)
        pagemap[i] = NOT_IN_CORE;	/* no paged pages in core, yet */

      for (i = endlod; i < memreq; i++) {
        pagedesc[i].next = &pagedesc[i+1];	/* setup pointer chain */
        pagedesc[i].prev = &pagedesc[i-1];
        pagedesc[i].loc = (dataspace + (i * BLKSIZ));
        pagedesc[i].vpage = NO_PAGE;
        }
      i = memreq - 1;
      pagedesc[i].next = &pagedesc[endlod];	/* make the list circular */
      pagedesc[endlod].prev = &pagedesc[i];	/* excluding pre and extra */
      mru = &pagedesc[i];			/* init mru to last page */
      }
}

zipini()
{ /*	ZIPINI initializes the ZIL world's link to the interpreter.  Pointers
	to each of the tables referenced are setup.  Type of status line, and
	interpreter capabilities are setup (split screen).  All of this
	initialization requires loading only the first game block, which
	contains a 64 byte header of game file information.

	Break characters for opread are setup as well.  Since they are defined
	in the vocab table's header, this requires loading the entire preload.

	(EZIP -- set interpreter id and version)
  */
    short i;
    char *dest, *src2;
    ZIPINT count, source;

    if (GTVBYT(PVERS1) != ZMVERS) THEN	/* check z-machine */
      fatal("Wrong Z-Machine version");
    if (GTVBYT(PVERS2) & 1) THEN	/* check for byte swapped file */
      fatal("Byte swapped game file");
#ifdef ZIP
    if (GTVBYT(PVERS2) & 2) THEN	/* check for status line mode */
      timemd++;				/* indicate time mode requested */
    if (slflg == 0) THEN		/* status line available? */
      PTVBYT(PVERS2, (GTVBYT(PVERS2) | STATBIT)); /* bit set means no */
#elseif EZIP
    PTVBYT(PVERS2, EMODE);
#endif
#ifdef EXZIP
    PTVWRD(PSCRWD, (SCREEN_LENGTH << 8) | scrwid);
    if (graphics > 0) THEN
      PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FDISPL);	/* say we have graphics */
     else
      PTVWRD(PFLAGS, GTVWRD(PFLAGS) & (~FDISPL)); /* no graphics */
#endif
    zorkid = GTVWRD(PZRKID);		/* get zork id */
    voctab = GTVWRD(PVOCTB);		/* set up vocab pointer */
    objtab = GTVWRD(POBJTB);		/* and the object table ptr */
    glotab = GTVWRD(PGLOTB);		/* and the globals table */
    wrdtab = GTVWRD(PWRDTB);		/* and the fwords table */

    purbot = GTVWRD(PPURBT);		/* get the purebot pointer */
    if (purbot & BYTEBITS) THEN 
      purbot += BLKSIZ;			/* round up to next block */
    purbot >>= CVTBLK;			/* convert to blocks */
    
    count = GTVBYT(voctab);		/* first byte is number to transfer */
    source = voctab + 1;		/* source is an offset type ptr */
    dest = rbrks;
    for (i = 1; i <= count; i++) 	/* transfer si break chars */
      *dest++ = GTVBYT(source++);
    esibrks = dest;
    src2 = irbrks;			/* end list with initial breaks */
    do {
      *dest++ = *src2++;
      }
    while (*(src2 - 1) != 0);		/* transfer up to and inc a null */
    vwlen = GTVBYT(source++);		/* get vocab entry length */
    vwords = GTVWRD(source);		/* save number of words in vocab */
    vocbeg = source + 2;		/* set starting point of vocab tbl */

    mtime();				/* set up random seeds */

#ifdef EXZIP
    PTVWRD(PINTWD, INTID*256+INTVER);   /* set interpreter ID and VERsion */
    bufflg = 1;                         /* this flag relates to OPBUFOUT */
                                        /* and should start out being 1  */
					/* maybe it should also be set on RESTORE -ASK */
#endif
#ifdef EXZIP
    PTVBYT(PVERS2, highlight_bits);
   /* tell the game what this hunk-o-junk can do */
#endif
  restart(ZFALSE);			/* continue ... */
}

restart(midgame)
int midgame;				/* FALSE if called from zipini() */
{
  /*	Restart (also called by ZIPINI) reloads preload code, saves any flags
	that would be wiped out by the reload, and jumps to the game entry
	point.  (EZIP add in appropriate low memory settings.)
  */
    ZIPINT oldflags;
    ZIPBYT modebits;

    if (midgame) THEN {			/* reload preload, jim */
      oldflags = GTVWRD(PFLAGS);
      getpre(0, endlod);
      PTVWRD(PFLAGS, oldflags);		/* save flags (SCRIPT) across reload */

#ifdef EXZIP
      PTVWRD(PINTWD, INTID*256+INTVER); /* set interpreter ID and VERsion */
#endif
#ifdef EXZIP
      PTVBYT(PVERS2, highlight_bits);        /* remind game of capabilities */
#endif
}
    vidflg = 1;
    bufflg = 1;
    rdir = 0;
    modebits = GTVBYT(PVERS2);
    if (scroll && splitable) THEN	/* initialize the split bit */
      modebits |= SPLTBIT;
    else 
      modebits &= ~SPLTBIT;
    PTVBYT(PVERS2, modebits);

#ifdef ZIP
    toplin = STATLEN + 1;		/* reset screen parameters */
#endif
    spltflg = 0;
    screen = 0;
    linecnt = 0;
    winlen = 22;
    md_clr(0);				/* clear the screen */
    locate(25,1);
    zsp = zstack + LSTACK;		/* setup stack pointers */
    ssp = sstack + STKLEN;
    chrptr = outbuf;			/* reset output buffer pointers */
    if (scripting) THEN			/* reset script output buffer */
      p_chrptr = p_outbuf;
    zlocs = zsp - zstack;		/* make a locals pointer */
    zlocs--;	 			/* to next stack slot*/
    bspltb(GTVWRD(PSTART));		/* get starting address */
    zpc1 = zblk;
    zpc2 = zoff;
    newzpc();
    return;
}

/* find length of file by seeking to end */
long
filen(fd) int fd; {
    long curptr, endfile, lseek();
    curptr = lseek(fd, 0L, SEEK_CUR);	/* find current pos */
    endfile = lseek(fd, 0L, SEEK_END);	/* go to end */
    lseek(fd, curptr, SEEK_SET);	/* back to where we were */
    return(endfile);
}