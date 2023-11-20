/************************************************************************
*									*
*	S Y S T E M   D E P E N D E N T 				*
*									*
************************************************************************/
#include "zipdefs.h"
#include "extern.h"
#include "ctype.h"	/* for isprint() */

#include "stdlib.h"
#include "dos.h"
#include "conio.h"
#include "errno.h"
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <sys/timeb.h>
#include <time.h>

#define CURSOR_BLINK 20		/* 100ths of a second between blinks */

#define LOWFONT 3
#define HIGHFONT 6

union REGS regs;
struct SREGS sregs;

static int cursor_on = 0;

int cscrwid = 80;
int cscrhit = 24;

static char curs_save_top = 6;
static char curs_save_bot = 7;
static int curs_off = 0;
static int mouse_shows = 0;
static unsigned char cur_palette[4];	/* used on pcjr, tandy stuff...*/
static int lru_index = 1;		/* oddball stuff */

int compaq = 0;				/* if 1, no italics */

do_seek(fd, how, where)
int fd, how, where;
{
  regs.h.ah = 0x42;
  regs.h.al = how;
  regs.x.cx = 0;
  regs.x.dx = where;
  regs.x.bx = fd;
  int86(0x21, &regs, &regs); }

md_mouse(fcn, arg1, arg2, arg3, arg4)
int  fcn, arg2;
int (*arg1)();
short arg3, arg4;
{
  unsigned int data_seg;
  regs.x.ax = fcn;  /* set the register for proper call */
  switch(fcn) {  /* set the appropriate variables */
    case INIT_MOUSE:
      if (!mouse_on) return(0);
      data_seg = get_segreg(2);		/* get ds */
      int86(0x33, &regs, &regs);
      return(regs.x.ax);
    case SHOW_MOUSE:
      if (!mouse_shows)
        int86(0x33, &regs, &regs);
      mouse_shows = 1;
      if (graphics < 0) THEN {
        if (!curs_off) {
          regs.h.ah = 3;
	  int86(0x10, &regs, &regs);
          curs_save_top = regs.h.ch;
          curs_save_bot = regs.h.cl;
	  curs_off = 1; }
        regs.h.ah = 1;
        regs.h.ch = 0x20;
        regs.h.cl = 0;
        int86(0x10, &regs, &regs); }
      break;
    case HIDE_MOUSE:
      if (mouse_shows)
        int86(0x33, &regs, &regs); /* make the mouse call */
      mouse_shows = 0;
      break;
    case SET_MOUSE_INPUT_MASK:
      regs.x.cx = 0x4; /*arg1 input mask */
      regs.x.dx = arg1;	 /* address offset to routine */
      int86x(0x33, &regs, &regs, 3, get_segreg(1));
      break;
    case SET_MOUSE_Y_BOUNDS:
      regs.x.cx = (arg2 * 8);
      regs.x.dx = (arg3 * 8) - 9;  /* formerly - 1 */
      int86(0x33, &regs, &regs);
      break;
    case SET_MOUSE_TEXT_CURSOR:
      regs.x.bx = arg2;
      regs.x.cx = arg3;
      regs.x.dx = arg4;
      int86(0x33, &regs, &regs);
      break;
    case GET_MOUSE_POSITION:
      int86(0x33, &regs, &regs); /* make the mouse call */
      mouse_buttons = regs.x.bx; /* bit 0 left, bit 1 right, 1=down */
      mouse_cur_x = regs.x.cx;
      mouse_cur_y = regs.x.dx;
      break;
   case SET_MOUSE_POSITION:
     regs.x.cx = arg2;
     regs.x.dx = arg3;
     int86(0x33, &regs, &regs); /* make the mouse call */
     mouse_cur_x = arg2;
     mouse_cur_y = arg3;
     break;    
   } /*end switch*/
}

/* Call this with -1 to toggle the cursor, with 0 to kill it, with
   1 to turn it on (least useful operation) */
void md_write_cursor(state)
int state;
{ char tmpattr, curschar; 
  if (graphics < 0) THEN {
    if (!curs_off || (state == -1)) THEN
      return; }			/* don't mess with the cursor in text mode */
  if (state == -1) THEN {
    if (cursor_on == 0) THEN
      state = 1;
     else
      state = 0;
    }
  if (state != cursor_on) THEN {
    regs.h.ah = 9;
    regs.h.bh = 0;
    regs.x.cx = 1;
    if (graphics > 0) THEN {
      curschar = 0x80;
      tmpattr = curattr | 0x80; }
     else {
      if (state) THEN {
	curschar = 0xdb;
	tmpattr = curattr | 0x80; }
       else {
	curschar = ' ';
	tmpattr = curattr; } }
/*  tmpattr = ((curattr & 7) << 4) | ((curattr >> 4) & 7) | 0x80; */
    regs.h.al = curschar;
    regs.h.bl = tmpattr;
    if (cursor_on == 1) THEN
      cursor_on = 0;
     else
      cursor_on = 1;
    int86(0x10, &regs, &regs);
  }
}

void colorq()
{
  char ich;
  if (docolor != 0) return;
  md_clr(0);
  gamprnt("Do you want color? ");
  ich = md_getch();
  if ((ich == 'y') || (ich == 'Y')) THEN
    docolor = 1;
   else
    docolor = -1;
}

int scan_copyright(ptr,str)
unsigned char far *ptr;
unsigned char *str;
{
  unsigned char *savstr;
  unsigned char far *savptr;
  int maxscan = 256;
  savstr = str;
  while (maxscan-- > 0) {
    if (*ptr == *str) {
      savptr = ptr;
      while (1) {
        if (*++str == 0) return(1);
	if (*++ptr != *str) {
	  str = savstr;
	  ptr = savptr;
	  break; } } }
    ptr++; }
  return(0);
}

check_tandy()
{
  unsigned char far *idpointer;
  int s1, s2;
  char buf[0x40];
/* Page 4-14 of the PS/2 & PC BIOS Technical Reference describes
   the following method for providing "video function compatibility".
   1) Do int 10H/1AH, which only works on PS/2s.  Win immediate.
   2) If that didn't work, do int 10H/12H, return EGA information,
      which only works with EGA.
   3) Perform a presence test(?) on video buffer addresses 0b8000H
      & 0b0000H to determine which functions are present. */
  regs.h.ah = 0x1A;
  regs.h.al = 0;
  int86(0x10, &regs, &regs);	/* Read display combination code */
  if (regs.h.al == 0x1A) {	/* Function supported... */
    s1 = regs.h.bl;
    if (s1 & 1) {	/* Odd display code is mono ... */
      docolor = -1; }
    if ((s1 == 4) || (s1 == 8))
      ega = 1;
    else
      ega = 0;
    regs.h.ah = 0x1B;
    regs.x.bx = 0;
    regs.x.di = &buf[0];
    int86x(0x10, &regs, &regs, 3, get_segreg(2));
    cscrwid = ((int)buf[0]) & 0xff;
    cscrhit = ((int)buf[0x22]) & 0xff;	/* current width and height */
    return(ega); }		/* End of first test */
  regs.h.ah = 0x12;
  regs.h.bl = 0x10;
  int86(0x10, &regs, &regs);
  if (regs.h.bl != 0x10) {	/* Have EGA */
    ega = 1;
    regs.h.ah = 0x11;
    regs.h.al = 0x30;
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);
    cscrhit = regs.h.dl + 1;
    regs.h.ah = 0xf;
    int86(0x10, &regs, &regs);
    cscrwid = regs.h.ah;
    return(1); }		/* End of second test */
  /* Now check some other stuff */
  FP_SEG(idpointer) = 0xf000;
  FP_OFF(idpointer) = 0xfffe;
  if (*idpointer == 0xfd) {
    compaq = 1;
    return(1); }
   else {
    FP_OFF(idpointer) = 0xc000;
    if (*idpointer == 0x21) {
      if (scan_copyright(idpointer, "Tandy")) {
        compaq = 1;
	return(1); } } }
  return(0);
}

is_tandy()
{
  unsigned char far *idpointer;
  if (tandy != -1) return(tandy);
  tandy = 0;
  if (is_graphics()) {
    tandy = check_tandy();
    if (ega) THEN {
      colorq();
      return(1); }
    if (tandy) THEN {
      colorq();
      if (docolor != 1) THEN {
        tandy = 0; } }
     else {
      if (docolor == 1) THEN
        graphics = -1; } }
  return(tandy);
}

is_graphics()
{
  char grphbit;
  grphbit = GTVWRD(PFLAGS) & FDISPL;
  if ((grphbit && (graphics ==0)) || (graphics == 1)) {
    regs.h.ah = 0xf;
    int86(0x10, &regs, &regs);
    if (regs.h.al == 7) {
      graphics = -1;
      grphbit = 0;
      return(0); }
    if (grphbit)
      fontsize = 8;
    graphics = 1;
    return(1); }
  return(0);
}

int grphtab_munged = 0;
int grphtab_7c;
int grphtab_7e;

void md_init_screen(first)
int first;
{ char ich;
  int far *chrvptr;
  int save_es;
  struct SREGS segregs;
  unsigned char far *idpointer;
  char grphbit;
  grphbit = GTVWRD(PFLAGS) & FDISPL;
  FP_SEG(idpointer) = 0xf000;
  FP_OFF(idpointer) = 0xfffe;
  if ((*idpointer == 0x2d) || (*idpointer == 0x9a))
    compaq = 1;
  check_tandy();
  if (is_graphics()) THEN {		/* do graphics */
    if (!is_tandy()) {		/* if loser requested color, then maybe
				   disable graphics and try again */
      if ((docolor == 1) && (graphics != 1)) THEN {
	graphics = -1;
	md_init_screen(first);
	return; }
      docolor = -1;		/* don't do color, for now...*/
      }
#ifdef GERMAN
    language_ok = 0;
#endif
    graphics = 1;
    *alt_font = 0xff;
    *(alt_font + 1) = 0xff;
    *(alt_font + 2) = 0xff;
    *(alt_font + 3) = 0xff;
    *(alt_font + 4) = 0xff;
    *(alt_font + 5) = 0xff;
    *(alt_font + 6) = 0xff;
    *(alt_font + 7) = 0xff;		/* initialize special character that's
				   all 1s, for use in inverse video etc. */
/*  PTVWRD(PFLAGS, (GTVWRD(PFLAGS) | FDISPL)); */
    }
   else {
/*  PTVWRD(PFLAGS, (GTVWRD(PFLAGS) & (~FDISPL))); */
    graphics = -1;		/* definitely don't do graphics */
    fontsize = 1;
    }
  if (graphics > 0) {
    FP_SEG(chrvptr) = 0;
    FP_OFF(chrvptr) = 0x7c;
    grphtab_munged = 1;
    grphtab_7c = *chrvptr;
    *chrvptr = alt_font;
    FP_OFF(chrvptr) = 0x7e;
    grphtab_7e = *chrvptr;	/* make sure we save these things */
    *chrvptr = get_segreg(2); }	/* get data segment */
  if (first) {
    regs.h.ah = 15;		/* we're about to ask bios about display */
    int86(0x10, &regs, &regs);	/* current video state */
    oldmode = regs.h.al; }
  if (graphics < 0) THEN {	/* not doing graphics */
    if (docolor == 0) THEN {	/* are we allowed to do color? */
      int86(0x11, &regs, &regs);
      if ((regs.x.ax & 0x30) != 0x30) THEN {
	colorq(); }
       else
	docolor = -1;		/* used to do colorq if oldmode = 2 or 3 */
      }
    }
  regs.h.ah = 0;
  if (graphics > 0) THEN {	/* graphics mode */
    cscrwid = 80;
    cscrhit = 24;
    if (ega) THEN {
      if (docolor == -1) THEN {
	scrmode = 0x6;
	normattr = 0x01; }
       else {
	scrmode = 0xe;
	normattr = (bgcolor << 4) | fgcolor; } }
    else if (tandy) THEN {		/* funny four-color stuff */
      scrmode = 0xa;
      normattr = 0x03; }
    else {
      scrmode = 6;
      normattr = 0x01; }	/* white-on-black */
    if (tandy && !ega)
      highlight_bits = 0x3d;
     else if (ega && (docolor > 0))
      highlight_bits = 0x3d;
     else
      highlight_bits = 0x3a;
    }
   else if (docolor > 0) THEN {
    if (ega && ((oldmode == 0xe) || (oldmode == 0x10) || (oldmode == 0x12))) {
	scrmode = oldmode; }
      else {
        scrmode = 3;
	cscrhit = 24;
	cscrwid = 80; }
    normattr = (bgcolor << 4) | fgcolor;
    highlight_bits = 0x35;	/* not supporting DISPLAY or underline */
    }
   else {			/* black and white text */
    if (ega && ((oldmode == 7) || (oldmode == 0xf) || (oldmode == 0x11)))
	scrmode = oldmode;
      else {
	scrmode = 2;
	cscrhit = 24;
	cscrwid = 80; }
    normattr = 0x07;
    if (compaq)
      highlight_bits = 0x34;	/* no display, color, or underline */
     else
      highlight_bits = 0x3c;	/* no display or color */
    }
  regs.h.al = scrmode;
  int86(0x10, &regs, &regs);	/* into highres graphics, or whatever */
  if (docolor > 0) THEN {
    if (ega) THEN {
      if (graphics > 0) THEN {
	regs.h.ah = 0x11;
        regs.h.al = 0x30;
        regs.h.bh = 1;
        int86(0x10, &regs, &regs);
        save_es = get_segreg(3);
        FP_SEG(chrvptr) = 0;
        FP_OFF(chrvptr) = 0x10c;
        do_copy(save_es, *chrvptr, get_segreg(2), &main_font[0], 128*regs.x.cx);
        zdumpfont(&main_font[0]); }
      regs.h.ah = 0x10;
      regs.h.al = 1;
      regs.h.bh = bgcolor;
      int86(0x10, &regs, &regs);	/* set overscan register */ }
    else {
      cur_palette[0] = bgcolor;
      regs.h.ah = 0xb;		/* this sets the border/background */
      regs.h.bh = 0;
      regs.h.bl = bgcolor;
      int86(0x10, &regs, &regs);
      if (tandy) {		/* only true if four-color graphics */
        cur_palette[3] = fgcolor;	/* initialize some stuff */
        cur_palette[1] = 0xff;
        cur_palette[2] = 0xff;
        regs.h.ah = 0x10;	/* and set color 3 to be the foreground */
        regs.h.al = 0;
        regs.h.bh = fgcolor;
        regs.h.bl = 3;
        int86(0x10, &regs, &regs); } } }
   else if (graphics > 0) THEN {
    if (ega) {
      zusrfont(alt_font);
      regs.h.ah = 0x10;
      regs.h.al = 0;
      regs.h.bl = 0;
      regs.h.bh = 0;
      int86(0x10, &regs, &regs);	/* set color 0?? */
      regs.h.ah = 0x10;
      regs.h.al = 0;
      regs.h.bl = 1;
      regs.h.bh = 7;
      int86(0x10, &regs, &regs); }
    else {
      regs.h.ah = 0xb;
      regs.h.bh = 0;
      regs.h.bl = 7;
      int86(0x10, &regs, &regs);		/* Set FG for CGA */ }
   }
  regs.h.ah = 0xF;
  int86(0x10, &regs, &regs);
  RM = regs.h.ah;
  scrwid = RM - LM;		/* now get screen width */
  slpp = cscrhit;
  curattr = normattr;
  tty_busted = 1;
  get_disk_info();
}

int md_getch()
{ regs.h.ah = 0;
  int86(0x16, &regs, &regs);
  return(regs.x.ax);
}

static unsigned int rseed1;
static unsigned int rseed2;

unsigned char md_cvtchr(chr)
int chr;
{
  unsigned char val;
  int lowch = chr & 0xff;	/* get low byte */
  if (lowch != 0) THEN {	/* normal key */
    if (lowch >= ' ') THEN {
      if (lowch < 127) THEN
	val = lowch;
       else
	val = '\b';
       }
     else if ((lowch == '\r') || (lowch == '\n')) THEN
      val = '\r';
     else if (lowch == '\b') THEN
      val = lowch;
     else if (lowch == 3) THEN
      z_exit();
     else
      val = 0;
     }
    else			/* extended key */
     switch (chr >> 8) {
	case 72: val = 129;	/* uparrow */
		 break;
	case 80: val = 130;	/* downarrow */
		 break;
	case 75: val = 131;	/* leftarrow */
		 break;
	case 77: val = 132;	/* rightarrow */
		 break;
	case 59: case 60: case 61: case 62: case 63: case 64: case 65:
	case 66: case 67: case 68:
		 val = (chr >> 8) + 74;
		 break;		/* f1 through f10 */
	/* others later */
	default: val = 0;	/* lower in ezip */
	}
  rseed1 += val;
  rseed2 ^= rseed1;
  return(val);
}

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
   if (mouse_event_count > 0) THEN {
     if (chr = do_mouse_event(&mouse_frob[0])) {
       linecnt = 0;		/* zero line counter, since we got a char...*/
       return(chr); } }
   if (kbhit()) THEN {	/* see if keystroke waiting */
     linecnt = 0;		/* zero line counter */
     if (chr = md_cvtchr(md_getch())) THEN {
       md_write_cursor(0);	/* make sure cursor off */
       return(chr);		/* return character if good */
       }
     md_sound(FEEP);
     }
    else {
     elapsed = md_timed_out();	/* returns elapsed time in 100ths */
     if ((elapsed - old_elapsed) >= CURSOR_BLINK) THEN {
       if (time == 0) THEN {
	 old_elapsed = 0;
	 md_inittime(); }
	else {
         old_elapsed = elapsed; }
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
    flush_buffer();
    }
  if (attrib == 0) THEN {
    curattr = normattr;
    current_highlight = 0;
    }
   else {
    attrib = attrib & (~MONO_SPACE) & (~current_highlight);
    if ((attrib & BOLD) && (!(highlight_bits & 4)))
      attrib ^= BOLD;
    if ((attrib & UNDER_LINE) && (!(highlight_bits & XUNDE)))
      attrib ^= UNDER_LINE;
    if (!attrib) return;
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



md_setup()
{  /* 	Setup performs any system dependent initialization that must be
	done only once.
   */
    int crterr();
    regs.x.dx = crterr;
    regs.h.ah = 0x25;
    regs.h.al = 0x24;
    int86x(0x21, &regs, &regs, 2, get_segreg(1));
    return;
}

mtime()
{  /* mtime get the machine time for setting the random seed.
   */
    regs.h.ah = 0x2c;
    int86(0x21, &regs, &regs);	/* time of day */
    if (regs.x.cx == 0) THEN {
      rseed1 = 0x8d0e;
      rseed2 = 0x9f81; }
     else {
      rseed1 = regs.x.cx;
      rseed2 = regs.x.dx; }
}

int rand()
{
  unsigned int temp;
  int flg = rseed1 & 1;
  temp = rseed1;
  rseed1 = rseed1 >> 1;
  rseed2 = rseed2 >> 1;
  if (flg) rseed2 = rseed2 | 0x8000;
  rseed1 ^= rseed2;
  rseed2 = temp;
  return(rseed1 & 0x7fff);
}


md_clr(scrno)
int scrno;
{ int scrtop,scrbot;
  int do_locate = 0, do_border = 0;
  int mouse_state = mouse_shows;
  if (mouse_state)
    md_mouse(HIDE_MOUSE);
  if (scrno == screen) THEN {		/* current screen? */
    do_locate = 1;			/* we have to move the cursor */
    if (scrno == 1) THEN {
      scrx = 0;				/* to top left if screen 1 */
      scry = 0;
      }
     else {
      scrx = LM;			/* to top left of screen 0 */
      scry = spltflg;
      if (spltflg == 0) THEN do_border = 1;
      linecnt = 0;
      }
    }
   else {				/* don't move the cursor, but */
    if (scrno == 1) THEN {		/* update the saved cursor pos */
      scr1x = 0;
      scr1y = 0;
      }
     else {
      linecnt = 0;			/* clear the more counter */
      scr0x = LM;
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
  if ((docolor > 0) && do_border) THEN {
    if (ega) {
      regs.h.ah = 0x10;		/* overscan register */
      regs.h.al = 1;
      regs.h.bh = bgcolor; }
    else {
      regs.h.ah = 0xb;
      regs.h.bh = 0;
      regs.h.bl = bgcolor; }
    int86(0x10, &regs, &regs);
    if (tandy == 1) {
      load_palette(); }
    }	/* and fix the border */
  if (do_locate) THEN
    locate(scry,scrx);
  if (mouse_state)
    md_mouse(SHOW_MOUSE);
}

locate(row, col)
short row,col;
{  /*	Uses ansi calls to position the cursor.
   */
   scrx = col;
   scry = row;
   regs.h.ah = 2;
   regs.h.dh = row;
   regs.h.dl = col;
   regs.h.bh = 0;
   int86(0x10, &regs, &regs);
}

/* this does cr without scripting.  Everybody except routines that explicitly
   don't want scripting should call mcrlf, which does scripting. */
md_mcrlf()
{  /* machine dependent (actually vt100) method for doing windowed scrolling.
   */
    int mouse_state = mouse_shows;
    if (screen == 0) THEN {		/* do work for screen 0 */
      chars_on_line = 0;		/* make sure line wrapping OK */
      if (!vidflg) THEN
	return;
      if (scry >= slpp) THEN {		/* special case if at end */
	if (mouse_state)
	  md_mouse(HIDE_MOUSE);
        regs.h.ah = 6;			/* scroll up */
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
        locate(slpp,LM);
	if (mouse_state)
	  md_mouse(SHOW_MOUSE);
        }
       else {
        locate(++scry,LM);		/* just move the cursor down */
	md_clear_eol();			/* and clear to eol, for fun */
	}
      linecnt++;			/* more counter */
      if (linecnt >= winlen) THEN {	/* this probably can't happen */
        md_printstr("[MORE]");		/* except at screen bottom */
        md_tinp(0,0);
	md_write_cursor(0);
	locate(scry,LM);
        md_clear_eol();
        linecnt = 1;
        }
      }
    else {				/* screen 1 requires no scroll */
      if (!vidflg) THEN
	return;
      if ((scry + 1) < spltflg) THEN
        locate(++scry,0);		/* need to aos vertical position */
     }
}

mcrlf()
{
  if ((screen == 0) && scripting)
    md_script_cr();
  md_mcrlf();
}

md_clear_eol()
{
  regs.h.ah = 9;
  regs.h.bh = 0;
  regs.x.cx = scrwid - scrx;
  regs.h.al = ' ';
  regs.h.bl = normattr;
  int86(0x10, &regs, &regs);
}

md_printstr(str)
char str[255];
{ char chr;
  int offs;
  union REGS charout_regs;
  union REGS curset_regs;
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
  


static long inittime;		/* in 100ths */

long get_time()
{
/* 100ths since midnight */
  regs.h.ah = 0x2c;
  int86(0x21, &regs, &regs);
  return(((long)regs.h.dl) + 100L * ((long)regs.h.dh +
					 60L * (long)regs.h.cl +
					 3600L * (long)regs.h.ch));
}

void md_inittime()
{
  inittime = get_time();
}

int md_time_diff(itime)
long itime;
{ long ctime = get_time();
  if (itime > ctime) THEN { /* must have gone past midnight */
    ctime = itime - ctime; }
   else
    ctime = ctime - itime;
  return((short)(ctime & 0xffff));
}

md_wait(secs)
int secs;
{
  long lsecs = (long)secs * 100;
  long itime = get_time();
  while (1) {
    if (md_time_diff(itime) >= lsecs) break; }
}

int md_timed_out()
{
  return(md_time_diff(inittime));
}

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
	(XZIP.  Look at PTCHARS table to see if character terminates...)
	Line counter is zeroed on every character read.
   */
    int c;
    int elapsed;
    int old_elapsed = 0;
    md_inittime();
    if (cnt > curscrwid) THEN		/* don't allow hardware scroll */
      cnt = curscrwid - 1;
    md_write_cursor(1);			/* cursor on */
    while (ZTRUE) {
      while (ZTRUE) {
        if (mouse_event_count > 0) THEN {
	  c = do_mouse_event(&mouse_frob[0]);
	  break; }
	if (kbhit()) THEN {
		/* read a character if it's there */
          c = md_cvtchr(md_getch());
          break;
          }
         else
	  elapsed = md_timed_out();	/* elapsed time in 100ths */
	  if ((elapsed - old_elapsed) >= CURSOR_BLINK) THEN {
	    if (timeout == 0) THEN {
	      old_elapsed = 0;
	      md_inittime(); }
	     else
	      old_elapsed = elapsed;
	    md_write_cursor(-1);
	    }
	  if (timeout && ((elapsed / 10) >= timeout)) THEN {
	    md_write_cursor(0);
	    return(-1 -i);
	    }
	} /* end of while true */
/* Now have a character in c, presumably structured as in technical manual */
      linecnt = 0;				/* zero line counter now */
      if ((c != EOL) && (c != CR) && not_terminator(c)) THEN
       {					/* a real character */
        if (c != BKSPC) THEN {			/* backspace is special */
          if (isprint(c)) THEN {		/* printable */
	    if (i < cnt) THEN {			/* there's room */
	      *(buf + i) =c;		/* printable chars get saved... */
	      i++;
	      md_write_cursor(0);	/* cursor off */
#ifdef GERMAN
	      cvt_putc(c);
#else
	      md_putc(c);		/* and echoed, of course */
#endif
	      md_write_cursor(1);	/* cursor back on */
	      }
	     else			/* there isn't room */
	     { md_write_cursor(0);
#ifdef GERMAN
	       cvt_putc(c);
#else
	       md_putc(c);
#endif
	       md_sound(FEEP);
	       md_erase_char();
	       md_write_cursor(1);
	       }
	    }				/* end of if isprint THEN */ 
	    else if (c != -1) THEN
	      md_sound(FEEP);
	    }				/* end of if BKSPC THEN */
	  else if (i) THEN {		/* handle a backspace */		
	    i--;		
	    *(buf + i) = NULL;	/* wipe out last char in buffer */
	    md_erase_char();		/* rubout the last char typed */
	    md_write_cursor(1);		/* get the cursor back */
	    }
	  else  			/* no room for backspace */
	    md_sound(FEEP);		/* left margin, so beep */
	  }				/* if not EOL THEN */
	 else {
	  md_write_cursor(0);
	  read_terminator = c;
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
   regs.h.ah = 9;
   regs.h.al = ' ';
   regs.x.cx = 1;
   regs.h.bl = normattr;
   int86(0x10, &regs, &regs);	/* now write a space */
}

static char font_offset[4] = {0,0,0,0};
static char font_bases[4] = {0,0,0,0};

#define TIMER_CONTROL 0x43
#define TIMER_DATA 0x42
#define PORTB_CONTROL 0x61
#define PORTB_SPEAKER 3

md_sound(id)
char id;
{  int duration,freq;
   long itime;
   switch (id) {
       case BEEP: case FEEP:
            duration = 5;	/* 50 milliseconds */
	    freq = 994;		/* produces 1200 Hz tone */
            break;
       case BOOP:
	    duration = 15;	/* 250 milliseconds */
	    freq = 2711;	/* 440 Hz tone */
            break;
       default:
            return;
       }
   outp(TIMER_CONTROL,0xb6);	/* something's coming */
   outp(TIMER_DATA,freq & 0xff);	/* low byte */
   outp(TIMER_DATA,freq>>8);		/* high byte */
   itime = get_time();		/* time now */
   outp(PORTB_CONTROL, inp(PORTB_CONTROL) | PORTB_SPEAKER);/* speaker on */
   while (md_time_diff(itime) < duration);
   outp(PORTB_CONTROL, inp(PORTB_CONTROL) & (~PORTB_SPEAKER)); 
}

md_putc(c)
char c;
{  /* 	Machine dependent write of a character.
	(EZIP will require multiple channels for output.)
   */
    if (scrfont != 1) THEN {
      if (graphics == 1) THEN
	c = c - font_offset[scrfont-LOWFONT] + 
	    font_bases[scrfont-LOWFONT] + 128; }
    regs.h.ah = 9;		/* write a character */
    regs.h.al = c;
    regs.x.cx = 1;
    regs.h.bh = 0;
    regs.h.bl = curattr;
    int86(0x10, &regs, &regs);
    if ((graphics > 0) && (current_highlight & REVERSE)) THEN {
      regs.h.ah = 9;
      regs.x.cx = 1;
      regs.h.bh = 0;
      regs.h.al = 0x80;		/* special all 1s character */
      regs.h.bl = curattr | 0x80;	/* XOR it in...*/
      int86(0x10, &regs, &regs);
      }
    if ((current_highlight & UNDER_LINE) && (graphics > 0)) {
      regs.h.ah = 9;
      regs.x.cx = 1;
      regs.h.al = '_';
      regs.h.bh = 0;
      regs.h.bl = curattr | 0x80;
      int86(0x10, &regs, &regs);
      }
    scrx++;
    regs.h.ah = 2;
    regs.h.bh = 0;
    regs.h.dh = scry;
    regs.h.dl = scrx;
    int86(0x10, &regs, &regs);
}

md_ttyres()
{
   int far *chrvptr;
   if (grphtab_munged) {
     FP_SEG(chrvptr) = 0;
     FP_OFF(chrvptr) = 0x7c;
     *chrvptr = grphtab_7c;
     FP_OFF(chrvptr) = 0x7e;
     *chrvptr = grphtab_7e; }
   grphtab_munged = 0;
   if (mouseflg) THEN {
     mouseflg = 0;
     regs.x.ax = 0;
     int86(0x33,&regs, &regs);
     if (curs_off) THEN {
       regs.h.ah = 1;
       regs.h.ch = curs_save_top;
       regs.h.cl = curs_save_bot;
       int86(0x10, &regs, &regs); } }	/* reset the mouse */
   if ((!tty_busted) || (oldmode == scrmode)) THEN
     return;				/* only fix up if needed */
   regs.h.ah = 0;
   regs.h.al = oldmode;
   int86(0x10, &regs, &regs);
   return;
}

/* copy from one HUGE buffer to another */

/* moved to do_copy in assembly language, for speed */

#define EPSILON 3
/* Kluge -- Binary search for memory requested --
    Microsoft (CRT0.ASM) clobbers _psp:2 to amount in dataspace */
    
char huge *halloc(pages)
unsigned int pages;
{
  char huge *retval;
  pages = pages * (BLKSIZ / 16);	/* number of paragraphs requested */
  regs.h.ah = 0x48;
  regs.x.bx = pages;
  int86(0x21, &regs, &regs);
  if (regs.x.cflag) THEN return(0L);
  FP_SEG(retval) = regs.x.ax;
  FP_OFF(retval) = 0;
  return(retval);
}

void hfree(ptr)
char huge *ptr;
{
  unsigned int segaddr = FP_SEG(ptr);
  regs.h.ah = 0x49;
  int86x(0x21, &regs, &regs, 3, segaddr);
}

md_msize(memasked) int memasked; {	/* argument in BLOCKS */
    char huge *dataspace;
    int trial, upper, lower;
/* printf("md_msize(%d:blks) = %ld bytes\n",memasked, membytes); */
    if (dataspace = halloc(memasked)) {
	hfree(dataspace);	/* give it back for later alloc */
	return(memasked);	/* all there */
	}
    upper = memasked;		/* have to search */
    lower = 0;
    while (upper != lower) {
	trial = (upper+lower) / 2;	/* try to get half way */
/*	printf("(%d, %d) Trial = %d BLKS...", 
		upper, lower, trial); */
	if (dataspace = halloc(trial)) {	/* got this much ? */
/*	    printf("got it: seg = %0x, off = %0x\n",
			FP_SEG(dataspace), FP_OFF(dataspace)); */
	    hfree(dataspace);
	    if ((upper-lower) < EPSILON) break;
	    lower = trial;
	    }
	else {
/*	    printf("nope\n"); */
	    upper = trial;
	    }
	}
    if (!dataspace) {
	fatal("Couldn't allocate enough memory.");
	}
    return(trial);
}



/* find and initialize a printer */
md_find_printer ()
{
  int far *prtptr;
  int prtbase = 0x408;
  char *prterr = 0;
  char *printer_defunct();
  int i;
  FP_SEG(prtptr) = 0;
  for (i = 0; i < 3; i++) {
   FP_OFF(prtptr) = prtbase + (2 * i);	/* table of printer cards */
   if (*prtptr) THEN {		/* maybe found a printer? */
     regs.h.ah = 2;		/* printer status */
     regs.x.dx = i;		/* or is this supposed to be *prtptr?? */
     int86(0x17, &regs, &regs);
     if (!(prterr = printer_defunct())) {
       regs.h.ah = 1;		/* initialize */
       regs.x.dx = i;
       int86(0x17, &regs, &regs);
       return(i+1);		/* used to check return here, but loses
				   sometimes, so don't bother... */ } } }
  mcrlf();
  if (prterr) THEN {
    gamprnt("***Your printer isn't ready: ");
    gamprnt(prterr);
    mcrlf();
    return(0); }
   else {
    gamprnt("***No printers found."); }
  mcrlf();
  gamprnt("Scripting disabled.");
  mcrlf();
  return(0);
}

char *printer_defunct()
{
  char prtstat = regs.h.ah;
  if (prtstat & 0x20) THEN
    return("out of paper.");
  if (prtstat & 0x08) THEN
    return("I/O error.");
  if (prtstat & 0x01) THEN
    return("timeout.");
  if ((prtstat & 0x10) == 0) THEN
    return("not selected.");
  return(0);
}

static char printbuf[80];
static int printbufl = 0;

md_close_script(fd)
int fd;
{
  if (printbufl > 0) THEN {
    far_write(fd, 0, &printbuf[0], printbufl); }
  printbufl = 0;
  zclose(fd); }

force_script = 0;

md_script_char(ch)
char ch;
{
  char *prterr;
  if (!scripting) THEN
    return;
  if (scrptfd > 0) THEN {
    printbuf[printbufl++] = ch;
    if ((printbufl == 80) || force_script) THEN {
     if ((far_write(scrptfd, 0, &printbuf[0], printbufl) != printbufl) ||
	 diskabt) THEN {
      script_off();
      force_script = 0;
      mcrlf();
      mcrlf();
      md_sound(BEEP);
      if (!diskabt) THEN
        gamprnt("Disk full? ");
      gamprnt("Scripting disabled.");
      mcrlf(); }
     printbufl = 0; }
     else {
      if (force_script) {
	regs.h.ah = 0xd;
	int86(0x21, &regs, &regs); }
      force_script = 0; } }
    else
     if (scrptfd < 0) THEN {
      if (dosprint) THEN {	/* print using dos call */
	printabt = 0;
	regs.h.ah = 0x05;	/* does this eventually want critical error */
	regs.h.dl = ch;		/* handler?? */
	int86(0x21, &regs, &regs);
	if (printabt) THEN {
	  script_off();
	  mcrlf();
	  md_sound(FEEP);
	  gamprnt("Scripting aborted.");
	  mcrlf(); }
	return; }
      while(ZTRUE) {
       regs.h.ah = 0;
       regs.h.al = ch;
       regs.x.dx = (-scrptfd) - 1;
       int86(0x17, &regs, &regs);
       if (regs.h.ah & 0x01) THEN {	/* timed out */
	 if (handle_printer_error(0)) THEN	/* this is what EZIP checks */
	   continue;
	  else
	   break; }
        else
	 break; } }
}

handle_printer_error(str)
char *str;
{
  char ich;
  scripting = 0;
  mcrlf();
  gamprnt("Printer error: ");
  if (str)
    gamprnt(str);
   else
    gamprnt("not ready.");
  while (ZTRUE) {
    mcrlf();
    gamprnt(" Type R to retry, A to abort scripting: ");
    ich = md_inp();
#ifdef GERMAN
    cvt_putc(ich);
#else
    md_putc(ich);
#endif
    if ((ich == 'R') || (ich == 'r')) THEN {
      scripting = 1;
      return(1); }
    if ((ich == 'A') || (ich == 'a')) THEN {
      script_off();
      return(0); }
    md_sound(FEEP); }
}

md_script_cr()
{
  if (scrptfd < 0) THEN {
    md_script_char(CR);
    md_script_char(EOL); }
   else {
    md_script_char(CR);
    md_script_char(EOL); }
}

md_set_color(fg,bg)
char fg,bg;
{
  int new_palette = 0;
  int i;
  int clrwrd = GTVWRD(PCLRWRD);
  fg = zip_to_ibm_color[fg];
  bg = zip_to_ibm_color[bg];
  if (fg == (char)-1) THEN
    fg = fgcolor;
   else if (fg == (char)-2) THEN
          fg = zip_to_ibm_color[(clrwrd & 0xff)];
  if (bg == (char)-1) THEN
    bg = bgcolor;
   else if (bg == (char)-2) THEN
	  bg = zip_to_ibm_color[((clrwrd >> 8) & 0xff)];
  fgcolor = fg;
  bgcolor = bg;
  if (tandy) {
    if (bg != cur_palette[0]) THEN {
      new_palette = 1;
      cur_palette[0] = bg;
      regs.h.ah = 0x10;
      regs.h.al = 1;
      regs.h.bh = bg;
      int86(0x10, &regs, &regs); }
    bg = 0;
    fg = 0;
    for (i = 1; i <= 3; i++) { /* see if color's already there */
      if (cur_palette[i] == fgcolor) {
	fg = i;
	break; } }
    if (fg == 0) { /* not in palette, so clobber a slot */
      i = lru_index;
      if (++lru_index == 4)
	lru_index = 1;
      fg = i;
      new_palette = 1;
      cur_palette[fg] = fgcolor;
      regs.h.ah = 0x10;
      regs.h.al = 0;
      regs.h.bl = fg;
      regs.h.bh = fgcolor;
      int86(0x10, &regs, &regs); }
    if (new_palette) THEN
      load_palette(); }
  normattr = (bg << 4) | fg;
  curattr = (curattr & 0x88) | (normattr & 0x77);
}

load_palette()
{
  int i;
  for (i = 0; i < 4; i++) {
    if (cur_palette[i] < 16) {
      regs.h.ah = 0x10;
      regs.h.al = 0;
      regs.h.bh = cur_palette[i];
      regs.h.bl = i;
      int86(0x10, &regs, &regs); } }
}

/* see font_bases */		/* starting offset */
static char font_sizes[4] = {0,0,0,0};		/* number of characters */
/* see font_offset */ /* to get real char in font */
static char font_order[4] = {0,0,0,0};
static char fonts_loaded = 0;
static char slots_left = 127;
#define FIRST_FREE 1

/* fonts are stored at the end of the datfile, starting at datend.
   There are fontlen bytes, which can be any number of fonts.  When
   a new font is loaded, we just seek to datend, and search for the
   appropriate one.  The 34-byte header has as its first word the
   font id, then 32 bytes of font header.  The first word gives the
   number of characters actually defined, which is sufficient (since
   each character is 8 bytes) to tell us how many bytes to skip if
   this guy isn't it. */

md_set_font(fontno)
int fontno;
{  int fontcount;
   int oldfont = scrfont;		/* this is the return value */
   int fontheader[17];
   char lfont;
   if (fontno == 1) THEN {		/* normal font, nothing special */
     scrfont = 1;
     return(oldfont); }
   if (fontno == 2) THEN
     return(0);				/* don't support picture font */
   if (graphics < 1) THEN {
     if (fontno != 3) THEN return(0);
     scrfont = fontno;
     return(oldfont); }
   if (fontno > HIGHFONT) THEN
     return(0);				/* four font max? */
   if (font_bases[fontno - LOWFONT]) THEN {	/* already loaded */
     scrfont = fontno;
     return(oldfont); }
   lseek(gamechn, datend, SEEK_SET);	/* seek to first font */
   while (ZTRUE) {
     if ((far_read(gamechn, 0, fontheader, 34) != 34) ||
	 diskabt ||
	 (fontheader[3] != 0x0808) ||
	 ((fontcount = fontheader[1]) > 127))
       return(0);			/* bad font area */
     if (fontheader[0] != fontno) THEN
       lseek(gamechn, (long)(fontcount * 8), SEEK_CUR);
      else
       break; }
/* if we get here, we found the font */
   fontno = fontno - LOWFONT;
   font_offset[fontno] = fontheader[2];	/* sub this from char before outputting */
   while (fontcount > slots_left) {
     lfont = font_order[--fonts_loaded] - LOWFONT; /* last font */
     font_bases[lfont] = 0;		/* clear the information */
     font_offset[lfont] = 0;
     slots_left += font_sizes[lfont];	/* this is how much room now */
     font_sizes[lfont] = 0;
     font_order[fonts_loaded] = 0; }
  font_order[fonts_loaded++] = fontno + LOWFONT;
  font_sizes[fontno] = fontsize;
  font_bases[fontno] = FIRST_FREE + (127 - slots_left);
  far_read(gamechn, 0, alt_font + (8 * font_bases[fontno]), fontcount * 8);
  if (ega) {
    regs.h.ah = 0;
    regs.h.al = scrmode;
    int86(0x10, &regs, &regs);
    if (docolor > 0) THEN
      zdumpfont(&main_font[0]);
    else {
      zusrfont(alt_font);
      regs.h.ah = 0x10;
      regs.h.al = 0;
      regs.h.bl = 1;
      regs.h.bh = 7;
      int86(0x10, &regs, &regs); } }
  slots_left = slots_left - fontcount;
  scrfont = fontno + LOWFONT;
  return(oldfont);
}


long lseek(handle, offset, type)
int handle;
long offset;
int type;
{
  regs.h.ah = 0x42;
  regs.h.al = type;
  regs.x.bx = handle;
  regs.x.cx = (short)(offset >> 16);
  regs.x.dx = (short)(offset & 0xffff);
  int86(0x21, &regs, &regs);
  offset = ((long)regs.x.dx << 16) | regs.x.ax;
  return(offset);
}

/* Get information about whether current drive is hard drive (in
   which case we start out with a save default that doesn't include
   a drive letter), and how many floppy drives there are (so we can
   give a swap disk message if needed, thus eliminating the "last"
   case where DOS will dump shit on the screen for us...)*/

int dos_version = 0;
int cur_drive;
int nfloppies = 0;
char defpath[PATHSIZ];

get_disk_info()
{
   unsigned char far *foo;
   int ndrives;
   defpath[0] = 0;
   int86(0x11, &regs, &regs);	/* equipment list */
   if (regs.x.ax & 1)
     nfloppies = ((regs.x.ax >> 6) & 3) + 1;	/* number of floppy drives */
   regs.h.ah = 0x30;
   int86(0x21, &regs, &regs);
   dos_version = regs.h.al;	/* s.b. 2 or 3... */
   regs.h.ah = 0x19;
   int86(0x21, &regs, &regs);	/* get current drive */
   cur_drive = regs.h.al;	/* this is 0 based--A would be 0, C 2 */
   if (get_disk_type(0) == -1) { /* connected drive is hard drive? */
     defdrive[0] = 0; }		/* default to current drive */
    else {			/* default to some other floppy */
     defdrive[0] = ((cur_drive + 1) ^ 3) + 'A' -1;
     defdrive[1] = ':';
     defdrive[2] = 0; }
}

/* return -1 if hard disk, 1 if floppy, 0 if not valid.  Drive number
   is 0 for default, 1 for A, etc. */
get_disk_type(drive)
int drive;
{
   int dataseg = get_segreg(2);
   int ndataseg;
   unsigned char far *foo;
   if (dos_version == 2) {
     regs.h.ah = 0x1c;
     regs.h.dl = drive;
     ndataseg = int86x(0x21, &regs, &regs, 2, dataseg);
     if (regs.h.al == 0xff) return(0);	/* bad drive */
     FP_SEG(foo) = ndataseg;
     FP_OFF(foo) = regs.x.bx;
     if (*foo == 0xf8)
       return(-1);		/* -1 means hard drive */
      else
       return(1); }		/* 1 means floppy */
    else {
     regs.h.ah = 0x44;		/* use IOCTL call instead */
     regs.h.al = 0x08;
     regs.h.bl = drive;
     int86(0x21, &regs, &regs);
     if (regs.x.cflag) return(0);	/* bad drive...*/
     if (regs.x.ax) return(-1);
     return(1); }	/* -1 means hard drive */
}

get_fname_drive(fname)
char *fname;
{
  unsigned char fcb[40];
  int dataseg = get_segreg(2);
  regs.h.ah = 0x29;
  regs.h.al = 0;
  regs.x.si = fname;
  regs.x.di = &fcb[0];
  int86x(0x21, &regs, &regs, 3, dataseg);	/* parse file name */
  if (regs.x.cflag || (regs.h.al == 0xff))
    return(-1);
  return(fcb[0]);
}

md_display_default(fname, oname)
char *fname, *oname;
{
  char *sname;
  int i = 0;
  sname = oname;
  while (defdrive[i])
    *oname++ = defdrive[i++];
  i = 0;
  while (defpath[i])
    *oname++ = defpath[i++];
  while (*oname++ = *fname++);
  gamprnt(sname); }

md_install_default(fname,target)
char *fname, *target;
{
  int drive;
  char *tstr, *lslash, *dirstr;
  lslash = 0;
  drive = get_fname_drive(fname);
  if (drive < 0) return;		/* bad file name, don't use it */
  if (drive > 0) {			/* drive was specified */
    defdrive[0] = drive + 'A' - 1;
    defdrive[1] = ':';
    defdrive[2] = 0;
    while (1) {
     if (*fname++ == ':') break; } }
  tstr = fname;				/* point after drive spec */
  while (*tstr) {
    if (*tstr == '\\') lslash = tstr;	/* look for directory delimiters */
    tstr++; }
  if (lslash) {				/* there was a directory */
    tstr = fname;			/* point to beginning */
    dirstr = &defpath[0];		/* remember it here */
    while (1) {
      *dirstr++ = *tstr;		/* copy a char */
      if (tstr == lslash) {		/* are we done? */
        *dirstr = 0;			/* null-terminated */
	break; }
      tstr++; }
    fname = lslash+1; }			/* point to beginning of file name */
  while (*target++ = *fname++);		/* and copy it */
}

md_parse_file(usrnam, outnam, defnam)
char *usrnam, *outnam, *defnam;
{
  char scratch[PATHSIZ];
  char *sptr, *lslash, *sfname, *dirptr;
  int drive = get_fname_drive(usrnam);	/* check for drive spec */
  int res = 0;
  lslash = 0;
  sptr = &scratch[0];
  if (drive < 0) return(0);
  if (drive > 0) {			/* there was one */
    defpath[0] = 0;
    usrnam += 2;			/* flush it from the front */
    *sptr++ = drive + 'A' -1;		/* and use it */
    *sptr++ = ':'; }
   else if (defdrive[0]) {		/* otherwise use the default, maybe */
    res = 1;
    *sptr++ = defdrive[0];
    *sptr++ = ':'; }
  sfname = usrnam;
  while (*usrnam) {			/* look for directory part */
    if (*usrnam == '\\')			/* found something */
      lslash = usrnam;
    usrnam++; }
  if (lslash) {				/* found directory part */
    *lslash = 0;			/* null-terminate it */
    dirptr = sfname;			/* point to beginning */
    usrnam = lslash + 1; }		/* point past end */
   else {
    if (defpath[0])
      res = 1;
    dirptr = defpath;			/* use default */
    usrnam = sfname; }
  while (*dirptr) {			/* copy directory part */
    *sptr++ = *dirptr;
    dirptr++; }
  if (lslash)
    *sptr++ = '\\';
  if (!(*usrnam)) {		/* if no file part */
    usrnam = defnam;
    res = 1; }
  while (*sptr++ = *usrnam++);
  sptr = &scratch[0];
  while (*outnam++ = *sptr++);
  return(res); }

int cur_floppy = -1;
int prev_floppy = 0;

md_check_swap(fname)
char *fname;
{
  int drive = get_fname_drive(fname);
  if (drive < 0) return;		/* bad drive spec */
  if (swap_to_floppy && (gamechn > 0) && (!no_paging)) {
    zclose(gamechn);			/* in case loser swaps without us */
    regs.h.ah = 0xd;
    int86(0x21, &regs, &regs);
    gamechn = 0; }
  if ((drive != 0) && (drive != (cur_drive + 1)) && (drive != cur_floppy) &&
      (get_disk_type(drive) != -1) && (nfloppies == 1)) {
    /* there should be a problem only when we're not saving to the
       current drive, the drive we're saving to is a floppy, and
       there's only one floppy on the system. */
    prev_floppy = cur_floppy;
    cur_floppy = drive;
    PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FSTAT);
    swapped = 1; }
}

md_check_unswap()
{
  if (swapped && floppy_script) {
    force_script = 1;
    md_script_char(' '); }
  md_get_game_chan(0);
  if (swapped && (floppy_script || ((get_disk_type(0) != -1) &&
      (!no_paging) && swap_to_floppy))) {
    cur_floppy = prev_floppy;
    PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FSTAT); }
  swapped = 0;
}

md_get_game_chan(force)
int force;
{
  char tmpbuf[1];
  if ((gamechn == 0) && (force || (!no_paging))) {
    while(1) {
      regs.h.ah = 0xd;
      int86(0x21, &regs, &regs);
      gamechn = zopen(gamfile, RDONLY);
      if (gamechn < 0) {
        PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FSTAT);
	gamprnt("Please put your PLAY diskette in drive ");
	if (swapdrive == 0)
	  md_putc(cur_drive + 'A');
	 else
	  md_putc(swapdrive + 'A' -1);
	md_putc('.');
	mcrlf();
	gamprnt("Press any key when ready: ");
	md_inp();
	mcrlf(); }
       else {
	far_read(gamechn,0,tmpbuf,1);
	break; } } }
}