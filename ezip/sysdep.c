/************************************************************************
*									*
*	S Y S T E M   D E P E N D E N T 				*
*									*
************************************************************************/
#include "zipdefs.h"
#include "extern.h"
#include "ctype.h"	/* for isprint() */

#ifdef IBMPC
#include "stdlib.h"
#include "dos.h"
#include "malloc.h"
#include "conio.h"
#endif
#ifdef EXZIP
#include <sys/types.h>
#include <sys/timeb.h>
#include <time.h>
#endif

#define CURSOR_BLINK 15		/* 100ths of a second between blinks */

union REGS regs;
union REGS curset_regs;
union REGS charout_regs;

static int tty_busted = 0;

static int cursor_on = 0;
static char alt_font[1024];

/* Call this with -1 to toggle the cursor, with 0 to kill it, with
   1 to turn it on (least useful operation) */
void md_write_cursor(state)
int state;
{ char tmpattr; 
  if (graphics < 0) THEN
    return;			/* don't mess with the cursor in text mode */
  if (state == -1) THEN {
    if (cursor_on == 0) THEN
      state = 1;
     else
      state = 0;
    }
  if (state != cursor_on) THEN {
    charout_regs.h.ah = 9;
    charout_regs.h.bh = 0;
    charout_regs.x.cx = 1;
    charout_regs.h.al = 0x80;
/*  tmpattr = ((curattr & 7) << 4) | ((curattr >> 4) & 7) | 0x80; */
    tmpattr = curattr | 0x80;
    charout_regs.h.bl = tmpattr;
    if (cursor_on == 1) THEN
      cursor_on = 0;
     else
      cursor_on = 1;
    int86(0x10, &charout_regs, &charout_regs);
  }
}

void md_init_screen()
{ char ich;
  void mchrv();
  struct SREGS segregs;
  char grphbit;
  grphbit = GTVWRD(PFLAGS) & FDISPL;
  if ((grphbit && (graphics == 0)) || (graphics == 1)) THEN { /* do graphics */
    docolor = -1;		/* don't do color, for now...*/
    graphics = 1;
    alt_font[0] = 0xff;
    alt_font[1] = 0xff;
    alt_font[2] = 0xff;
    alt_font[3] = 0xff;
    alt_font[4] = 0xff;
    alt_font[5] = 0xff;
    alt_font[6] = 0xff;
    alt_font[7] = 0xff;		/* initialize special character that's
				   all 1s, for use in inverse video etc. */
    segread(&segregs);		/* and stuff it into interrupt vector */
    mchrv(segregs.ds, &alt_font[0]);
/*  PTVWRD(PFLAGS, (GTVWRD(PFLAGS) | FDISPL)); */
    }
   else {
/*  PTVWRD(PFLAGS, (GTVWRD(PFLAGS) & (~FDISPL))); */
    graphics = -1;		/* definitely don't do graphics */
    }
  regs.h.ah = 15;		/* we're about to ask bios about display */
  int86(0x10, &regs, &regs);	/* current video state */
  RM = regs.h.ah;		/* number of columns on screen */
  scrwid = RM - LM;
  oldmode = regs.h.al;
  if (graphics < 0) THEN {	/* not doing graphics */
    if (docolor == 0) THEN {	/* are we allowed to do color? */
      if ((oldmode == 3) || (oldmode == 2)) THEN {	/* ask, you fool */
	printf("Do you want color? ");
	ich = md_getch();
	if ((ich == 'y') || (ich == 'Y')) THEN
	  docolor = 1;
	 else
	  docolor = -1;
	}			/* end of if oldmode==3 */
       else
	docolor = -1;		/* don't do color */
      }
    }
  regs.h.ah = 0;
  if (graphics > 0) THEN {	/* graphics mode */
    scrmode = 6;
    normattr = 0x07;		/* white-on-black */
#ifdef EZIP
    highlight_bits = 0x3b;	/* here's what we can do */
#else
    highlight_bits = 0x3a;
#endif
    }
   else if (docolor > 0) THEN {
    scrmode = 3;
    normattr = 0x17;
#ifdef EZIP
    highlight_bits = 0x37;	/* no underline available */
#else
    highlight_bits = 0x35;	/* not supporting DISPLAY or underline */
#endif
    }
   else {			/* black and white text */
    scrmode = 2;
    normattr = 0x07;
#ifdef EZIP
    highlight_bits = 0x3f;
#else
    highlight_bits = 0x3c;	/* no display or color */
#endif
    }
  regs.h.al = scrmode;
  int86(0x10, &regs, &regs);	/* into highres graphics, or whatever */
  curattr = normattr;
  tty_busted = 1;
}

#ifdef EXZIP
int md_getch()
{ regs.h.ah = 0;
  int86(0x16, &regs, &regs);
  return(regs.x.ax);
}
#endif

#ifdef EZIP
char md_cvtchr(chr)
int chr;
{
  int lowch = chr & 0xff;	/* get low byte */
  if (lowch != 0) THEN {	/* normal key */
    if (lowch >= ' ') THEN {
      if (lowch < 127) THEN
	return(lowch);
       else
	return('\b');
       }
     else if ((lowch == '\r') || (lowch == '\n')) THEN
      return('\r');
     else if (lowch == '\b') THEN
      return(lowch);
     else if (lowch == 3) THEN
      z_exit();
     else
      return(0);
     }
    else			/* extended key */
     switch (chr >> 8) {
	case 72: return(14);	/* uparrow */
	case 75: return(11);	/* left arrow */
	case 77: return(7);	/* right arrow */
	case 80: return(13);	/* downarrow */
	default: return(0);	/* lower in ezip */
	}
}
#endif

#ifdef EXZIP
md_inp()
{
  return(md_tinp(0,0));
}

/* we rewrite the cursor every 1/10 second, regardless of anything
   else, but only if we're in graphics mode */
md_tinp(time, handler)
ZIPINT time, handler;
{
  void md_inittime();
  int md_timed_out();
  int elapsed;
  int old_elapsed = 0;
  char chr;
  md_inittime();		/* initialize */
  md_write_cursor(1);		/* turn cursor on */
  while (ZTRUE) { 
   if (kbhit()) THEN {	/* see if keystroke waiting */
     if (chr = md_cvtchr(md_getch())) THEN {
       md_write_cursor(0);	/* make sure cursor off */
       return(chr);		/* return character if good */
       }
     printf(FEEP);
     }
    else {
     elapsed = md_timed_out();	/* returns elapsed time in 100ths */
     if ((elapsed - old_elapsed) >= CURSOR_BLINK) THEN {
       old_elapsed = elapsed;
       md_write_cursor(-1);
       }
     if (time && ((elapsed / 10) > time)) THEN {	/* Have we timed out? */
       if (internal_call(handler)) THEN {	/* abort requested? */
	 md_write_cursor(0);	/* cursor off */
         return;
	 }
	old_elapsed = 0;
	md_inittime();		/* start timeout over */
        }
      }
    }				/* end of while true */
}

md_hlite(attrib)
ZIPINT attrib;
{ if (bufflg && (chrptr != outbuf)) {	/* if buffering on, flush the buffer */
    putchr(Z_EOL);
    dumpbuf();
    } 
  if (attrib == 0) THEN {
    curattr = normattr;
    current_highlight = 0;
    }
   else {
    attrib = attrib & (~MONO_SPACE) & (~current_highlight);
    current_highlight = current_highlight | attrib;
    if (graphics < 0) THEN {
      if (docolor > 0) THEN {
        if (attrib & REVERSE) THEN
          curattr = ((curattr >> 4) & 7) | ((curattr & 7) << 4);
	if (attrib & BOLD) THEN
	  curattr = curattr | 8;	/* intense foreground */
	}				/* underlining done manually */
       else {				/* black and white */
	if (attrib & REVERSE) THEN
	  curattr = 0x70;
	if (attrib & BOLD) THEN
	  curattr = curattr | 8;
	if (attrib & UNDER_LINE) THEN
	  curattr = (curattr & 0xf8) | 1;
	}
      }
    }
}

#endif


md_setup()
{  /* 	Setup performs any system dependent initialization that must be
	done only once.
   */
#ifdef SYSTEM5
    signal(SIGINT, z_exit);	/* handle errors without user logout */
    signal(SIGQUIT, z_exit);

    md_initty(); 		/* turn off echo and unbuffer input */
#endif /* SYSTEM5 */
    return;
}

mtime()
{  /* mtime get the machine time for setting the random seed.
   */
    long time(), tloc = 0;

    rseed = time(tloc);		/* get system time */
    srand(rseed);		/* get a random seed based on time */
    return;
}

md_clr(scrno)
int scrno;
{ int scrtop,scrbot;
  int do_locate = 0;
  if (scrno == screen) THEN {		/* current screen? */
    do_locate = 1;			/* we have to move the cursor */
    if (scrno == 1) THEN {
      scrx = 0;				/* to top left if screen 1 */
      scry = 0;
      }
     else {
      scrx = 0;				/* to top left of screen 0 */
      scry = spltflg;
      }
    }
   else {				/* don't move the cursor, but */
    if (scrno == 1) THEN {		/* update the saved cursor pos */
      scr1x = 0;
      scr1y = 0;
      }
     else {
      scr0x = 0;
      scr0y = spltflg;
      }
    }
  regs.h.ah = 6;	/* scroll up */
  regs.h.al = 0;	/* blank the whole window */
  regs.h.cl = 0;	/* always start on left edge */
  regs.h.dl = scrwid;	/* and end on right edge */
  if (scrno == 1) THEN {	/* top screen */
    regs.h.ch = 0;	/* start at top line */
    regs.h.dh = spltflg -1;	/* end at bottom */
    }
   else {
    regs.h.ch = spltflg;
    regs.h.dh = slpp;
    }
  if (graphics > 0) THEN
    regs.h.bh = 0;
   else
    regs.h.bh = normattr;
  int86(0x10, &regs, &regs);	/* do the scroll */
  if (do_locate) THEN
    locate(scry,scrx);
}

locate(row, col)
short row,col;
{  /*	Uses ansi calls to position the cursor.
   */
/*   int x,y; */		/* this was for debugging */
   scrx = col;
   scry = row;
/*   curset_regs.h.ah = 3;
   curset_regs.h.bh = 0;
   int86(0x10, &curset_regs, &curset_regs);
   x = curset_regs.h.dl;
   y = curset_regs.h.dh; */
   curset_regs.h.ah = 2;
   curset_regs.h.dh = row;
   curset_regs.h.dl = col;
   curset_regs.h.bh = 0;
   int86(0x10, &curset_regs, &curset_regs);
}

mcrlf()
{  /* machine dependent (actually vt100) method for doing windowed scrolling.
   */
    if (screen == 0) THEN {		/* do work for screen 0 */
      chars_on_line = 0;		/* make sure line wrapping OK */
      if (scry >= slpp) THEN {		/* special case if at end */
        regs.h.ah=6;			/* scroll up */
        regs.h.al = 1;			/* one line */
        regs.h.ch = spltflg;		/* CHANGE to top line of screen 0 */
        regs.h.cl = 0;
        regs.h.dh = slpp;
        regs.h.dl = scrwid;
	if (graphics > 0) THEN
	  regs.h.bh = 0;
	 else
	  regs.h.bh = normattr;
        int86(0x10, &regs, &regs);
        locate(slpp,0);
        }
       else {
        locate(++scry,0);		/* just move the cursor down */
	md_clear_eol();			/* and clear to eol, for fun */
	}
      linecnt++;			/* more counter */
      if (linecnt >= winlen) THEN {	/* this probably can't happen */
        md_printstr("[MORE]");		/* except at screen bottom */
        md_tinp(0,0);
	md_write_cursor(0);
	locate(scry,0);
        md_clear_eol();
        linecnt = 1;
        }
      }
    else {				/* screen 1 requires no scroll */
      if ((scry + 1) < spltflg) THEN
        locate(++scry,0);		/* need to aos vertical position */
     }
}

md_clear_eol()
{
  charout_regs.h.ah = 9;
  charout_regs.h.bh = 0;
  charout_regs.x.cx = scrwid - scrx;
  charout_regs.h.al = ' ';
  charout_regs.h.bl = normattr;
  int86(0x10, &charout_regs, &charout_regs);
}

md_printstr(str)
char str[255];
{ char chr;
  int offs;
  charout_regs.h.bh = 0;
  charout_regs.h.ah = 9;
  charout_regs.x.cx = 1;
  charout_regs.h.bl = curattr;
  curset_regs.h.bh = 0;
  curset_regs.h.ah = 2;
  curset_regs.h.dh = scry;
  curset_regs.h.dl = scrx;
  for (offs = 0; chr = str[offs++];) {
    charout_regs.h.al = chr;
    int86(0x10, &charout_regs, &regs);
    curset_regs.h.dl++;
    int86(0x10, &curset_regs, &regs);
    }
  scrx = curset_regs.h.dl;
}
  

#ifdef EXZIP
static struct timeb inittime, curtime;

void md_inittime()
  {
  void ftime ();
  ftime(&inittime);	/* Read the starting time */
  }

int md_timed_out()
  { int milldiff;
    int secdiff; 
    ftime(&curtime);		/* Time now */
    if (curtime.millitm >= inittime.millitm) THEN
      milldiff = curtime.millitm - inittime.millitm;
     else
      milldiff = -(inittime.millitm - curtime.millitm);
    milldiff = (milldiff / 10) + (curtime.time - inittime.time)*100;
    return(milldiff);
}
#endif

int md_getl(buf, cnt, i, timeout)
char *buf;
int cnt;
int i;
int timeout;
{  /*	Machine (or OS) dependent line read.  Md_getl reads chars up to cnt.
	All unprintables or escape sequences are thrown away.  When the
	cnt'th char is typed, it echoes, disappears, and the terminal beeps.
	Backspaces are handled by backing up, printing a space and backing 
	up again.  The number of chars actually read is returned.  

	(EZIP.  This will have to be fixed to allow for internal call on
	timeout.   It also should be able to take input from alternate
	channels.)
   */
    int c;
    int elapsed;
    int old_elapsed = 0;
    md_inittime();
    if (cnt > scrwid) THEN		/* don't allow hardware scroll */
      cnt = scrwid - 1;
    md_write_cursor(1);			/* cursor on */
    while (ZTRUE) {
      while (ZTRUE) {
        if (kbhit()) THEN {
		/* read a character if it's there */
          c = md_cvtchr(md_getch());
          break;
          }
         else
	  elapsed = md_timed_out();	/* elapsed time in 100ths */
	  if ((elapsed - old_elapsed) >= CURSOR_BLINK) THEN {
	    old_elapsed = elapsed;
	    md_write_cursor(-1);
	    }
	  if (timeout && ((elapsed / 10) >= timeout)) THEN {
	    md_write_cursor(0);
	    return(-1 -i);
	    }
        } /* end of while true */
/* Now have a character in c, presumably structured as in technical manual */
      if ((c != EOL) && (c != CR)) THEN {	/* a real character */
        if (c != BKSPC) THEN {			/* backspace is special */
          if (isprint(c)) THEN {		/* printable */
	    if (i < cnt) THEN {			/* there's room */
	      *(buf + i) =c;		/* printable chars get saved... */
	      i++;
	      md_write_cursor(0);	/* cursor off */
	      md_putc(c);		/* and echoed, of course */
	      md_write_cursor(1);	/* cursor back on */
	      }
	     else			/* there isn't room */
	     { md_putc(c);
	       printf(FEEP);
	       md_erase_char();
	       md_write_cursor(1);
	       }
	    }				/* end of if isprint THEN */ 
	    else if (c != -1) THEN
	      printf(FEEP);
	    }				/* end of if BKSPC THEN */
	  else if (i) THEN {		/* handle a backspace */		
	    i--;		
	    *(buf + i) = NULL;	/* wipe out last char in buffer */
	    md_erase_char();		/* rubout the last char typed */
	    md_write_cursor(1);		/* get the cursor back */
	    }
	  else  			/* no room for backspace */
	    printf(FEEP);		/* left margin, so beep */
	  }				/* if not EOL THEN */
	 else {
	  md_write_cursor(0);
	  mcrlf();
	  break;
	  }
         }				/* end of while loop */
    md_write_cursor(0);			/* cursor off */
    *(buf + i) = NULL;			/* make an end string */
/* I don't think this crlf should exist - ASK */
    return(i);
}

md_erase_char()
{  md_write_cursor(0);		/* make sure cursor off */
   scrx--;			/* back up cursor position */
   locate(scry,scrx);		/* and move it there */
   charout_regs.h.ah = 9;
   charout_regs.h.al = ' ';
   charout_regs.x.cx = 1;
   charout_regs.h.bl = normattr;
   int86(0x10, &charout_regs, &charout_regs);	/* now write a space */
}

md_putc(c)
char c;
{  /* 	Machine dependent write of a character.
	(EZIP will require multiple channels for output.)
   */
    charout_regs.h.ah = 9;		/* write a character */
    charout_regs.h.al = c;
    charout_regs.x.cx = 1;
    charout_regs.h.bl = curattr;
    int86(0x10, &charout_regs, &regs);
    if ((graphics > 0) && (current_highlight & REVERSE)) THEN {
      charout_regs.h.al = 0x80;		/* special all 1s character */
      charout_regs.h.bl = curattr | 0x80;	/* XOR it in...*/
      int86(0x10, &charout_regs, &regs);
      }
    if ((current_highlight & UNDER_LINE) && (graphics > 0)) {
      charout_regs.h.al = '_';
      charout_regs.h.bl = curattr | 0x80;
      int86(0x10, &charout_regs, &regs);
      }
    scrx++;
    curset_regs.h.ah = 2;
    curset_regs.h.bh = 0;
    curset_regs.h.dh = scry;
    curset_regs.h.dl = scrx;
    int86(0x10, &curset_regs, &regs);
}

#ifdef SYSTEM5

md_initty()
{  /* 	This routine performs Unix tty magic.  It sets the input buffer
	length to 0, and turns off canonization and echo. 
   */
    struct termio ttyinfo;

    ttyfd = fileno(stdin);		/* get a file descriptor */
    if (ioctl(ttyfd, TCGETA, &ttyinfo) == -1) THEN
      printf("\nIOCTL - TCGETA failed");
    ttyinfo.c_lflag &= ~ICANON;
    ttyinfo.c_lflag &= ~ECHO;	
    ttysav = ttyinfo.c_cc[VMIN];
    ttyinfo.c_cc[VMIN] = 0;
    if (ioctl(ttyfd, TCSETA, &ttyinfo) == -1) THEN
      printf("\nIOCTL - TCSETA failed");
    
}

md_ttyres()
{  /* 	This undoes the above magic.
   */
    struct termio ttyinfo;

    ioctl(ttyfd, TCGETA, &ttyinfo);
    ttyinfo.c_lflag |= ICANON;
    ttyinfo.c_lflag |= ECHO;
    ttyinfo.c_cc[VMIN] = ttysav;
    ioctl(ttyfd, TCSETA, &ttyinfo);
    close(ttyfd); 
}

#endif /* SYSTEM5 */

#ifdef IBMPC

md_ttyres()
{
   if ((~tty_busted) || (oldmode == scrmode)) THEN
     return;				/* only fix up if needed */
   regs.h.ah = 0;
   regs.h.al = oldmode;
   int86(0x10, &regs, &regs);
   return;
}

/* read into a ZIPCHAR (huge) buffer; just use read except for the PC */

/*                  A T T E N T I O N                                 */
/* the problem is that a game file is not an integral number of       */
/* (512 byte) blocks long.					      */
/* when HREAD returns it passes the total number of bytes read        */
/* since this is != BLKSIZ the caller thinks we had a problem         */
/*      I think that by seting total = BLKSIZ                         */
/* I will bypass the error, but I may be hiding valid errors. I don't */
/* know for shure						      */
/* - ASK  (22 july)                                                   */

hread(fd, buf, count)
 int fd;
 ZIPCHAR *buf;
 int count;
 {
    char lbuf[BLKSIZ];
    int nread, total = 0;
    while (count >= BLKSIZ) {
	nread = read(fd, lbuf, BLKSIZ);
	LtoHcopy(lbuf, buf, nread);
	total += nread;
	if (nread != BLKSIZ) {
	    count = 0;			/* prevent further attempts */
	    total = BLKSIZ; /* ! ! ! here is my frob - ASK */
	    break;
	    }
	buf += BLKSIZ;
	count -= BLKSIZ;
	}
    if (count) {
	nread = read(fd, lbuf, count);
	LtoHcopy(lbuf, buf, nread);
	total += nread;
	}
    return(total);
}

/* write from a ZIPCHAR (huge) buffer; just use write except for the PC */

hwrite(fd, buf, count) int fd; ZIPCHAR *buf; int count; {
    char lbuf[BLKSIZ];
    int nwrote, total = 0;
    while (count >= BLKSIZ) {		/* transfer full blocks */
	HtoLcopy(lbuf, buf, BLKSIZ);	/* copy to local buffer for write */
	nwrote = write(fd, lbuf, BLKSIZ);
	total += nwrote;
	if (nwrote != BLKSIZ) {
	    count = 0;			/* prevent further attempts */
	    break;
	    }
	buf += BLKSIZ;
	count -= BLKSIZ;
	}
    if (count) {
	HtoLcopy(lbuf, buf, count);
	nwrote = write(fd, lbuf, count);
	total += nwrote;
	}
    return(total);
}

/* copy from a local buffer to a HUGE buffer */
		
LtoHcopy(lbuf, buf, count) char *lbuf; ZIPCHAR *buf; int count; {
    if (count > 0) do {
	*buf++ = *lbuf++;	/* from lbuf to buf:HUGE */
	} while (--count);
}

/* copy from a huge buffer to a local buffer */

HtoLcopy(lbuf, buf, count) char *lbuf; ZIPCHAR *buf; int count; {
    if (count > 0) do {
	*lbuf++ = *buf++;	/* from buf:HUGE to lbuf */
	} while (--count);
}


#define EPSILON 3
/* Kluge -- Binary search for memory requested --
    Microsoft (CRT0.ASM) clobbers _psp:2 to amount in dataspace */
    
md_msize(memasked) int memasked; {	/* argument in BLOCKS */
    char huge *dataspace;
    int trial, upper, lower;
    long membytes;
    membytes = (long)memasked * BLKSIZ;	/* try for the whole thing */
printf("md_msize(%d:blks) = %ld bytes\n",memasked, membytes);
    if (dataspace = halloc(membytes, 1)) {
	hfree(dataspace);	/* give it back for later alloc */
	return(memasked);	/* all there */
	}
    upper = memasked;		/* have to search */
    lower = 0;
    while (upper != lower) {
	trial = (upper+lower) / 2;	/* try to get half way */
	membytes = (long)trial * BLKSIZ;
	printf("(%d, %d) Trial = %d BLKS...", 
		upper, lower, trial);
	if (dataspace = halloc(membytes, 1)) {	/* got this much ? */
	    printf("got it: seg = %0x, off = %0x\n",
			FP_SEG(dataspace), FP_OFF(dataspace));
	    hfree(dataspace);
	    if ((upper-lower) < EPSILON) break;
	    lower = trial;
	    }
	else {
	    printf("nope\n");
	    upper = trial;
	    }
	}
    if (!dataspace) {
	printf("Couldn't Allocate Enough Memory\n");
	fatal("md_msize");
	}
    return(trial);
}


#endif /* IBMPC */
#ifdef ATT7300

md_msize(nblks) int nblks; {
    return(nblks);		/* paged machine, pretend it's infinite */
}

#endif

/* allocate the space for preload and paging */

ZIPCHAR *md_alloc(nbytes)
long nbytes;
{  /*	Allocate a segment of size nbytes and return a pointer to it.
	If an error occurs, return a null pointer.
   */
#ifdef IBMPC
printf("md_alloc(%ld)\n",nbytes);
return(halloc(nbytes, 1));		/* huge alloc(nels, elsize) */

#else

return(malloc(nbytes));		/* NULL if error */
#endif
}
