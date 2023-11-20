/************************************************************************
*                                                                       *
*   S Y S T E M   D E P E N D E N T                                     *
*         Part II                                                       *
*                                                                       *
*************************************************************************/
#include "zipdefs.h"
#include "struct.h"
#include "extern.h"
#include "sysdep.h"

#include <ctype.h>   /* for isprint() */
#include <stdlib.h>
#include <dos.h>
#include <conio.h>
#include <errno.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <sys\timeb.h>
#include <time.h>
#include <dos.h>
#include <string.h>

/* globals shared between sysdeps */
char curs_save_top = 6;
char curs_save_bot = 7;
char curs_off = 0;
int palette_size = 4;
unsigned char cur_palette[16];   /* used on pcjr, tandy stuff...*/
int wid = 0;                     /* for md_erase_char of mcga */

#ifdef DO_TANDY
static int lru_index = 1;      /* oddball stuff */
#endif

void md_erase_char()
{  
   char savefg, swid;
    
   if ( Display & VID_MCGA )
   { /* do variable width font back up */
      Scrx -= wid;               /* backspace sets up wid */
      locate(Scry,Scrx);
      savefg = Fgcolor;       /* save fgcolor */
      Fgcolor = Bgcolor;      /* foul up fgcolor = bgcolor; */
      swid = CWIDTH( SPACE );      /* save width of space */
      FontWid[0] = wid;
      md_putc( SPACE );       /* erase with space */
      Fgcolor = savefg;       /* restore fgcolor */
      FontWid[0] = swid;      /* restore width */
      Scrx -= wid;            /* and go backwards */
      locate( Scry, Scrx );
   }
   else
   { /* if not MCGA, then fixed width font */
      locate( Scry, Scrx-Font_width ); 
		md_putc( SPACE );
		locate( Scry, Scrx-Font_width );
   }
}

static char font_offset[4] = {0,0,0,0};
static char font_bases[4] = {0,0,0,0};

#define TIMER_CONTROL 0x43
#define TIMER_DATA 0x42
#define PORTB_CONTROL 0x61
#define PORTB_SPEAKER 3

void md_sound(id)
char id;
{
   int duration,freq;
   long itime;

   switch (id)
   { /* what kind of sound would you like? */
   case BEEP:
   case FEEP:
      duration = 5;           /* 50 milliseconds */
      freq = 994;             /* produces 1200 Hz tone */
      break;
   case BOOP:
      duration = 15;          /* 250 milliseconds */
      freq = 2711;            /* 440 Hz tone */
      break;
   }

   outp(TIMER_CONTROL,0xb6);           /* something's coming */
   outp(TIMER_DATA,freq & 0xff);       /* low byte */
   outp(TIMER_DATA,freq>>8);           /* high byte */
   itime = get_time();                 /* time now */
   outp(PORTB_CONTROL, inp(PORTB_CONTROL) | PORTB_SPEAKER);/* speaker on */
   while (md_time_diff(itime) < duration)
      ;
   outp(PORTB_CONTROL, inp(PORTB_CONTROL) & (~PORTB_SPEAKER)); 
}

void md_putc(c)
   unsigned char c;
{  /*    Machine dependent write of a character.
   (EZIP will require multiple channels for output.)
   */
   union REGS regs;

   if ( c < SPACE || c > CURSOR_CHAR )
   { /* check for special chars */
		if ( c > CURSOR_CHAR )
		{ /* highlight char */
			md_hlite( c & 0xf );
		}
      return;
   }

   if ( Display & VID_MCGA )
   { /* do special print for MCGA */
      mcga_putc( c );
      return;
   }

   if ( Scrfont != 1 && Scrfont != 4 )  
   { /* not normal or mono font, go get it */
      c -= (font_offset[Scrfont-LOWFONT] + font_bases[Scrfont-LOWFONT] + 128); 
   }

   if ( Display & VID_EGA )
   { /* use my EGA char display */
      ega_putc( c );
		locate( Scry, Scrx );		/* really move the cursor */
      return;
   }

	if ( Display & VID_CGA )
	{ /* use my CGA char display */
		cga_putc( c );
		locate( Scry, Scrx );		/* move cursor */
		return;
	}

   regs.h.ah = 9;      /* write a character */
   regs.h.al = c;
   regs.x.cx = 1;
   regs.h.bh = 0;
   regs.h.bl = Fgcolor;
   int86(0x10, &regs, &regs);
   if ( Win_hlmode & REVERSE )  
   { /* graphics reverse video */
      regs.h.ah = 9;
      regs.x.cx = 1;
      regs.h.bh = 0;
      regs.h.al = 0x80;      /* special all 1s character */
      regs.h.bl = Fgcolor | 0x80;      /* do XOR when copying */
      int86(0x10, &regs, &regs);
   }
   if ( Win_hlmode & UNDER_LINE )
   { /* graphics under line */
      regs.h.ah = 9;
      regs.x.cx = 1;
      regs.h.al = '_';
      regs.h.bh = 0;
      regs.h.bl = Fgcolor | 0x80;
      int86(0x10, &regs, &regs);
   }
   Scrx += Font_width;
   regs.h.ah = 2;          /* Set Cursor Location */
   regs.h.bh = 0;
   regs.h.dh = (unsigned char)(Scry/Font_height);
   regs.h.dl = (unsigned char)(Scrx/Font_width);
   int86(0x10, &regs, &regs);
}

/* pecial char print for EGA displays */
void ega_putc( c )
   char c;
{
   display_char10( c );

   Scrx += FONT_WIDTH;
}

void cga_putc( c )
	char c;
{
	display_char06( c );
	Scrx += FONT_WIDTH;
}

/* special char print out for variable width font used in MCGA */
void mcga_putc( c )
   char c;
{
   int char_off;
   int width, mwidth;
	int monospaced = 0;

	mwidth = CWIDTH(c);
	width = FontWid[c-0x20];

   if ( Win_hlmode & MONO_SPACE )
   { /* then just use mono space width */
      monospaced = Zip_font_width;
   }

   /* get offset into Font array, which starts @ Space char */
   char_off = (c - SPACE);

   display_char13( char_off, width, monospaced );

   Scrx += mwidth;       /* move "cursor" */

#ifdef C_MCGA
   for ( vert = char_off+Font_height; char_off < vert; char_off++)
   { /* for each horizontal line in blk. */
      mask = 0x80;         /* init. mask */   
      offst = (Max_screen_width*Scry+Scrx) + width;
      for (shft = offst-width; shft < offst; shft++)  
      { /* for each bit in defarry[] */
         if (mask & letarry[char_off])
         { /* pixel on, make foreground color */
            vb_ptr[shft] = Fgcolor;
         }
         else 
         { /* it's off, so turn it off */
            vb_ptr[shft] = Bgcolor;
         }
         mask >>= 1;
      }   
      Scry++;
   }
#endif
}
/* Kluge -- Binary search for memory requested --
   Microsoft (CRT0.ASM) clobbers _psp:2 to amount in dataspace */
    
char huge *halloc(pages)
unsigned int pages;
{
   pages = pages * (BLKSIZ / 16);   /* number of paragraphs requested */

   return( my_halloc( pages ) );
}
char huge *my_halloc( paragraphs )
   unsigned int paragraphs;
{
   char huge *retval;
   union REGS regs;   

   regs.h.ah = 0x48;
   regs.x.bx = paragraphs;
   int86(0x21, &regs, &regs);

   if (regs.x.cflag)
      return(0L);

   retval = MK_FP( regs.x.ax, 0);
   return retval;
}

void hfree(ptr)
   char huge *ptr;
{
   union REGS regs;   
   unsigned int segaddr = FP_SEG(ptr);

   regs.h.ah = 0x49;
   int86x(0x21, &regs, &regs, 1, segaddr);
}

md_msize( memasked )
   int memasked;     /* argument in BLOCKS */
{   
   char huge *dataspace;
   int trial, upper, lower;

   if ((dataspace = halloc(memasked)) != NULL)
   { /* we were able to get what we wanted */   
      hfree(dataspace);   /* give it back for later alloc */
      return(memasked);   /* all there */
   }

   upper = memasked;      /* have to search */
   lower = 0;
   while (upper != lower)
   { /* of course, you can't always get what you want */  
      trial = (upper+lower) / 2;   /* try to get half way */
      if ((dataspace = halloc(trial)) != NULL)
      { /* got this much */
         hfree(dataspace);
         if ((upper-lower) < EPSILON)
            break;
         lower = trial;
      }
      else
      { /* didn't get that, try again */
         upper = trial;
      }
   }

   if (!dataspace)
   { /* oh dear, unable to get any room! */
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
   union REGS regs;   

   prtptr = MK_FP( 0, 0 );

   for (i = 0; i < 3; i++)
   { /* check for printers com1-3 */   
      FP_OFF(prtptr) = prtbase + (2 * i);   /* table of printer cards */
      if (*prtptr)
      { /* maybe found a printer? */
         regs.h.ah = 2;      /* printer status */
         regs.x.dx = i;      /* which printer */
         int86(0x17, &regs, &regs);
         if ( (prterr = printer_defunct( regs.h.ah )) == 0 )
         { /* found it, so init it */   
            regs.h.ah = 1;      /* initialize */
            regs.x.dx = i;
            int86(0x17, &regs, &regs);
            return(i+1);
            
         }
      }
   }   
   mcrlf();
   if (prterr)
   { /* found a printer, but couldn't init it */   
      gamprnt("***Your printer isn't ready: ");
      gamprnt(prterr);
   }   
   else
   { /* no printers around */
      gamprnt("***No printers found.");
   }   
   mcrlf();
   gamprnt("Scripting disabled.");
   mcrlf();
   return(0);
}

char *printer_defunct( prtstat )
   int prtstat;
{
   if (prtstat & 0x20) 
      return("out of paper.");
   if (prtstat & 0x08) 
      return("I/O error.");
   if (prtstat & 0x01) 
      return("timeout.");
   if ((prtstat & 0x10) == 0) 
      return("not selected.");

   return(0);
}

static char printbuf[PRT_BUFF_LEN];
static int printbufl = 0;

void md_close_script(fd)
int fd;
{
   if (printbufl > 0)
   { /* somehting there so write it out */   
      far_write( fd, 0, (unsigned)&printbuf[0], (unsigned)printbufl);
   }   
   printbufl = 0;
   zclose(fd);
}   

static int force_script = 0;

void md_script_char(ch)
char ch;
{
   union REGS regs;   

   if (!Scripting || (!(Win_attributes & SCRIPTMASK)) ||
		(!isascii(ch) || (!isprint(ch) && !isspace(ch))) )
      return;

   if ( File_script )
   { /* sending script to a file */
      printbuf[printbufl++] = ch;
      if ((printbufl == PRT_BUFF_LEN) || force_script)  
      { /* full buffer or forcing me to write, so do it */
         if ((far_write(Scrptfd, 0, (unsigned)&printbuf[0], printbufl) != printbufl)
            || Diskabt)
         { /* problems doing file, shut it off and complain */
            script_off();
            force_script = ZFALSE;
            mcrlf();
            mcrlf();
            md_sound(BEEP);
            if (!Diskabt)
               gamprnt("Disk full? ");
            gamprnt("Scripting disabled.");
            mcrlf(); 
         }
         else if (force_script)
         { /* forced me to do it, so end it with a <CR> */
            regs.h.ah = 0xd;
            int86(0x21, &regs, &regs); 
            force_script = ZFALSE;
         }
         printbufl = 0;
      }
   }
   else if ( Scrptfd >= 0)  
   { /* sending to printer */
      if (Dosprint)  
      { /* print using dos call */
         Printabt = ZFALSE;
         regs.h.ah = 0x05;   /* does this eventually want critical error */
         regs.h.dl = ch;      /* handler?? */
         int86(0x21, &regs, &regs);
         if (Printabt)  
         { /* oh dear, we died */
            script_off();
            mcrlf();
            md_sound(FEEP);
            gamprnt("Scripting aborted.");
            mcrlf(); 
         }
      }
      else
      { /* use other thing */
         do
         { /* do it until we get it right */
            regs.h.ah = 0;
            regs.h.al = ch;
            regs.x.dx = Scrptfd - 1;
            int86(0x17, &regs, &regs);
         } while ( (regs.h.ah & 0x01) && handle_printer_error(0) );
      }
   }
}


int handle_printer_error(str)
char *str;
{
   char ich;

   Scripting = ZFALSE;
   mcrlf();
   gamprnt("Printer error: ");
   if (str)
      gamprnt(str);
   else
      gamprnt("not ready.");

   while (ZTRUE)
   {   
      mcrlf();
      gamprnt(" Type R to retry, A to abort Scripting: ");
      ich = (char)md_inp();
      if ( isprint( ich ) )
#ifdef GERMAN
         cvt_putc(ich);
#else
         md_putc(ich);
#endif
      if ((ich == 'R') || (ich == 'r'))
      { /* dumb user wants to retry */
         Scripting = 1;
      }   
      if ((ich == 'A') || (ich == 'a'))
      { /* dumb user wants to quit */
         script_off();
      }   
      md_sound(FEEP);
   }
   return Scripting;
}
/* send out CR/LF */
void md_script_cr()
{
   md_script_char(CR);
   md_script_char(EOL);
}

void md_set_color(fg,bg,id)
   int fg,bg;
   int id;
{
#ifdef DO_TANDY
   int i;
   union REGS regs;
#endif

   struct WINDOW *pwp;

   if ( id == -3 )
   { /* means use current screen */
		globals_to_window( Screen );
      id = Screen;
   }
   
   pwp = &wattrib[id];

   if ( fg > 0 )
   { /* change foreground color to passed color */
      if ( fg == 1 )
      { /* use system default */
         pwp->fgcolor = Def_fgcolor;
      }
		else
		{ /* use passed color */
      	pwp->fgcolor = Zip_to_ibm[fg-2];
		}
   }
   else if ( fg < 0 )
   { /* change to color cursor is sitting on */
      pwp->fgcolor = get_pixel_color( pwp->xcurpos+pwp->xpos,
			pwp->ycurpos+pwp->ypos );
   }
	pwp->zipfg = Ibm_to_zip[pwp->fgcolor];

   if ( bg > 0 )
   { /* make change to background color */
      if ( bg == 1 )
      { /* use default color */
         pwp->bgcolor = Def_bgcolor;
      }
		else
		{ /* used passed zip color */
      	pwp->bgcolor = Zip_to_ibm[bg-2];
		}
   }
   else if ( bg < 0 )
   { /* then use pixel color that is at current cursor pos */
      pwp->bgcolor = get_pixel_color( pwp->xcurpos+pwp->xpos,
			pwp->ycurpos+pwp->ypos );
   }

	pwp->zipbg = Ibm_to_zip[pwp->bgcolor];

#ifdef DO_TANDY
   if ( Display & VID_TANDY )
   { /* limited number of colors, see if it is around */
      for (i = 1, fg = 0; i <= (palette_size-1); i++) 
      { /* see if color's already there */
         if ( Tandy_palette[i] == pwp->fgcolor) 
         { /* found it! */
            fg = i;
            break; 
         } 
      }
      if ( fg == 0 ) 
      { /* not in palette, so clobber a slot */
         fg = lru_index++;
         if ( lru_index == palette_size )
         { /* start back at beginning */
            lru_index = 1;
         }
         Tandy_palette[fg] = pwp->fgcolor;
         regs.h.ah = 0x10;          /* Set Palette Registers */
         regs.h.al = 0;             /* Update Specified Palette Register */
         regs.h.bl = fg;            /* Palette Register Number */
         regs.h.bh = pwp->fgcolor;  /* Color Value */
         int86(0x10, &regs, &regs); 
      }
      if ( Tandy_palette[0] != pwp->bgcolor )
      { /* set new background color */
         Tandy_palette[0] = pwp->bgcolor;
         regs.h.ah = 0x10;          /* Set Palette Registers */
         regs.h.al = 0;             /* Update Specified Palette Register */
         regs.h.bl = 0;             /* Palette Register Number */
         regs.h.bh = pwp->bgcolor;  /* Color Value */
         int86( 0x10, &regs, &regs );
      }
   }
#endif

   if ( id == Screen )
   { /* make new globals */
      Fgcolor = pwp->fgcolor;
      Bgcolor = pwp->bgcolor;
   }
}
int get_pixel_color( x, y )
   int x,y;
{
   union REGS regs;

   if ( Display & VID_MCGA )
   { /* go look in vid mem directly */
      return( vb_ptr[(Max_screen_width*y+(x-1))] );
   }
   else
   { /* just use BIOS call */
      regs.h.ah = 0x0D;          /* Return Pixel Value */
      regs.h.bh = 0;             /* Video Page */
      regs.x.cx = x;             /* X pos */
      regs.x.dx = y;             /* Y pos */
      int86( 0x10, &regs, &regs );
      return( regs.h.al );       /* return pixel value */
   }
}
      


/* see font_bases */      /* starting offset */
static char font_sizes[4] = {0,0,0,0};      /* number of characters */
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

md_set_font(fontno,id)
int fontno, id;
{  
   int fontcount;
   unsigned char fontheader[17];
   char lfont;
   int oldfont, num;
   struct WINDOW *pwp;

   if ( fontno == 2 || fontno > HIGHFONT )
	{ /* no picture font or more than 4 */
      return(0);
	}

   if ( id == -3 || id == Screen )
   { /* use the current screen */
		globals_to_window( id );
      id = Screen;
   }
    
   pwp = &wattrib[id];
   
   oldfont = pwp->fontid;        /* this is the return value */
	pwp->fontid = fontno;

	if ( fontno == 2 )
	{
   	if (font_bases[fontno - LOWFONT])  
   	{ /* already loaded */
      	pwp->fontid = fontno;
      	return(oldfont); 
   	}
   	lseek(Gamechn, Datend, SEEK_SET);   /* seek to first font */
   	while (ZTRUE) 
   	{
      	num = far_read(Gamechn, 0, (unsigned)fontheader, FONT_HDR_SIZE);
      	if ( (num != FONT_HDR_SIZE) ||
         	Diskabt ||
         	(fontheader[3] != 0x08) ||
         	((fontcount = fontheader[1]) > 127))
         	return(0);         /* bad font area */

      	if (fontheader[0] != (unsigned char)fontno)
      	{ /* not the font we are looking for, try again */
         	lseek(Gamechn, (long)(fontcount * Font_width), SEEK_CUR);
      	}
      	else
         	break; 
   	}

   	/* if we get here, we found the font */
   	fontno = fontno - LOWFONT;
   	font_offset[fontno] = fontheader[2];   /* sub this from char before outputting */
   	while (fontcount > slots_left) 
   	{
      	lfont = font_order[--fonts_loaded] - LOWFONT; /* last font */
      	font_bases[lfont] = 0;      /* clear the information */
      	font_offset[lfont] = 0;
      	slots_left += font_sizes[lfont];   /* this is how much room now */
      	font_sizes[lfont] = 0;
      	font_order[fonts_loaded] = 0; 
   	}
   	font_order[fonts_loaded++] = (unsigned char)(fontno + LOWFONT);
   	font_sizes[fontno] = 8;
   	font_bases[fontno] = (unsigned char)(FIRST_FREE + (127 - slots_left));
   	far_read(Gamechn, 0, (unsigned)(Alt_font + (8 * font_bases[fontno])),
                  	fontcount * 8);
   	if ( !(Display & VID_MCGA) )
   	{ /* do special ROM font */
      	zdumpfont(&Main_font[0]);
      	zusrfont(Alt_font);
   	}   
   	slots_left = slots_left - fontcount;
   	pwp->fontid = fontno + LOWFONT;
	}

	window_to_globals( Screen );
   return(oldfont);
}


long lseek(handle, offset, type)
   int handle;
   long offset;
   int type;
{
   union REGS regs;   

   regs.h.ah = 0x42;
   regs.h.al = (unsigned char)type;
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

void get_disk_info()
{
   union REGS regs;   

   Defpath[0] = 0;

	int86(0x11, &regs, &regs);   /* equipment list */
   if (regs.x.ax & 1)
      Nfloppies = ((regs.x.ax >> 6) & 3) + 1;   /* number of floppy drives */

	regs.h.ah = 0x19;
   int86(0x21, &regs, &regs);   /* get current drive */
   Cur_drive = regs.h.al;   /* this is 0 based--A would be 0, C 2 */

	regs.h.ah = 0x30;
   int86(0x21, &regs, &regs);
   Dos_version = regs.h.al;   /* s.b. 2 or 3... */

	if (get_disk_type(0) == -1)
   { /* connected drive is hard drive */
      Defdrive[0] = 0; /* default to current drive */
   }   
   else
   { /* default to some other floppy */
      Defdrive[0] = (unsigned char)((Cur_drive + 1) ^ 3) + 'A' - 1;
      Defdrive[1] = ':';
      Defdrive[2] = 0;
   }   
}

/* return -1 if hard disk, 1 if floppy, 0 if not valid.  Drive number
   is 0 for default, 1 for A, etc. */
get_disk_type(drive)
int drive;
{
   union REGS regs;   
   int dataseg = get_segreg(2);
   int ndataseg;
   unsigned char far *foo;

   if (Dos_version == 2)
   { /* use special old-fashion call */
      regs.h.ah = 0x1c;
      regs.h.dl = (unsigned char)drive;
      ndataseg = int86x(0x21, &regs, &regs, 2, dataseg);
      if (regs.h.al == 0xff)
         return(0);   /* bad drive */
      foo = MK_FP( ndataseg, regs.x.bx );
      if (*foo == 0xf8)
      { /* hard drive */
         return(-1);
      }
      else
      { /* let's just say it's a floppy */
         return(1);
      }
   }
   else
   { /* use IOCTL call instead */
      regs.h.ah = 0x44;      					/* IOCTL */
      regs.h.al = 0x08;             		/* Is Changeable? */
      regs.h.bl = (unsigned char)drive; 	/* Drive Number */
      int86(0x21, &regs, &regs);
      if (regs.x.cflag && (regs.x.ax != 1) )
         return(0);   /* bad drive...*/
      else if (regs.x.ax)
		{ /* not changeable (i.e. hard disk/RAM disk) */
         return(-1);
		}
      else
		{ /* changeable (floppy) */
         return(1);
		}
   }
}

int get_fname_drive(fname)
   char *fname;
{
   unsigned char fcb[40];
   unsigned int dataseg = get_segreg( DS_REG );
   union REGS regs;   

   regs.h.ah = 0x29;									/* Parse File Name */
   regs.h.al = 0;										/* Parse Control */
   regs.x.si = (unsigned)fname;					/* String to Parse */
   regs.x.di = (unsigned)(&fcb[0]);				/* Unopened FCB Buffer */
   int86x(0x21, &regs, &regs, ES_REG, dataseg);
   if (regs.x.cflag || (regs.h.al == 0xff))
	{ /* problems, just return -1 */
      return(-1);
	}

	/* return drive number of fname */
   return( fcb[0] );
}
/* add default drive and path to the past fname */
void md_display_default(fname, oname)
   char *fname, *oname;
{
   char *sname;
   int i = 0;

   sname = oname;
   for( i=0; Defdrive[i]; i++)
      *oname++ = Defdrive[i];

   for( i=0; Defpath[i]; i++)
      *oname++ = Defpath[i];

   while ( (*oname++ = *fname++) != 0)
      ;

   gamprnt(sname);
}

/* parse passed name and make new defaults from it */
/* copy just the filename part into target */
void md_install_default(fname,target)
   char *fname, *target;
{
   int drive, reset_path = ZFALSE;
   char *tstr, *lslash, *dirstr;

   lslash = NULL;
   drive = get_fname_drive(fname);

   if (drive < 0)
   { /* doesn't have a good drive, so don't bother */
      return;
   }

   if (drive > 0)
   { /* drive was specified, so make it new default */
      Defdrive[0] = (unsigned char)(drive + 'A' - 1);
      Defdrive[1] = ':';
      Defdrive[2] = 0;
      while ( *fname++ != ':' )
         ; /* skip past drive parm */
		reset_path = ZTRUE;
   }

	for ( tstr = fname, lslash = NULL; *tstr; tstr++ )
	{ /* look for ending slash */
		if ( *tstr == '\\' )
			lslash = tstr;
	}

   if (lslash)
   { /* there was a directory */
      tstr = fname;                 /* point to beginning */
      dirstr = &Defpath[0];         /* remember it here */
      do
      { /* copy up to and including last slash */
         *dirstr++ = *tstr;         /* copy a char */
      } while ( tstr++ != lslash );
      *dirstr = '\0';
      fname = tstr;                 /* point to beginning of file name */
   }
	else if ( reset_path )
	{ /* change drives, so use clear old path */
		Defpath[0] = '\0';
	}
   while ( (*target++ = *fname++) != 0)
      ;  /* now copy in filename to passed parm */
}

/* taking a (possibly) fully-qualified usrnam, copy it to outnam, making
it fully qualified.  Defnam is used if there is no filename in usrnam.
Return TRUE if we used any default part, FALSE if usrnam was just fine */

int md_parse_file(usrnam, outnam, defnam)
   char *usrnam, *outnam, *defnam;
{
   char scratch[PATHSIZ];
	char workarea[PATHSIZ];
   char *sptr, *lslash, *sfname, *dirptr, *work;
   int drive = get_fname_drive(usrnam);   /* check for drive spec */
   int res = ZFALSE;
	int use_defpath = ZTRUE;

	/* make copy of usrnam, so we don't wack on it */
	strcpy( workarea, usrnam );
	work = workarea;

   lslash = NULL;
   sptr = &scratch[0];
   if (drive < 0)
   { /* bad drive specified, so don't bother */
      return(0);
   }

   if (drive > 0)
   { /* there was one passed */
		use_defpath = ZFALSE;
      work += 2;         /* skip over drive */
      *sptr++ = (unsigned char)(drive + 'A' - 1);      /* and use it */
      *sptr++ = ':';                   /* finish drive parm */
   }   
   else if (Defdrive[0])
   { /* otherwise use the default */
      res = ZTRUE;
      *sptr++ = Defdrive[0];
      *sptr++ = ':';                   /* finish drive parm */
   }

   /* now, check for passed directory part */
	for ( sfname = work, lslash = NULL; *work; work++ )
	{ /* look for ending slash */
		if ( *work == '\\' )
			lslash = work;
	}
   if ( lslash )
   { /* found directory part */
      *lslash = '\0';            /* null-terminate it */
      dirptr = sfname;           /* point to beginning */
      work = lslash + 1;       /* point past end */
   }
   else if ( use_defpath )
   { /* use default path, maybe */
      if ( Defpath[0] )
         res = ZTRUE;
      dirptr = Defpath;          /* use default */
      work = sfname;           /* point back to beginning of name */
   }
	else
	{ /* just make dirptr point to Null */
		dirptr = "";
      work = sfname;           /* point back to beginning of name */
	}

   while ( (*sptr = *dirptr++) != '\0' )
		sptr++; /* copy directory part */

   if (lslash)
   { /* put last slash back in */
      *sptr++ = '\\';
   }

   if ( *work == '\0' )
   { /* if no file part, so used passed default name */
      work = defnam;
      res = ZTRUE;
   }

	strcpy( sptr, work );
	strcpy( outnam, scratch );

   return(res);
}   

static int cur_floppy = -1;
static int prev_floppy = 0;

void md_check_swap(fname)
   char *fname;
{
   union REGS regs;   
   int drive = get_fname_drive(fname);

   if (drive < 0)
      return;      /* bad drive spec */

   if ( Swap_to_floppy )
   { /* using floppy, so get ready */
      zclose( Gamechn );         /* in case loser swaps without us */
		zclose( PFhandle );
      regs.h.ah = 0xd;						/* Reset Disk */
      int86(0x21, &regs, &regs);
      PFhandle = Gamechn = -1;
   }   
   if ((drive != 0) && (drive != (Cur_drive + 1)) && (drive != cur_floppy) &&
      (get_disk_type(drive) != -1) && (Nfloppies == 1))
   {   
    /* there should be a problem only when we're not saving to the
       current drive, the drive we're saving to is a floppy, and
       there's only one floppy on the system. */
      prev_floppy = cur_floppy;
      cur_floppy = drive;
      PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FSTAT);
      Swapped = ZTRUE;
   }   
}

void md_check_unswap()
{
   if (Swapped && Floppy_script)
   { /* Scripting to the floppy, so print a space */   
      force_script = ZTRUE;
      md_script_char(' ');
   }   
   md_get_game_chan( ZFALSE );
   if (Swapped && (Floppy_script || ((get_disk_type(0) != -1) &&
      (!No_paging) && Swap_to_floppy)))
   {   
      cur_floppy = prev_floppy;
      PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FSTAT);
   }   
   Swapped = 0;
}

void md_get_game_chan(force)
int force;
{
   if ((Gamechn < 0) && (force || (!No_paging)))
   { /* no game open, and need/forced to open it */   
		open_game_file();
   }
	reopen_pic_file();
}

void open_game_file()
{
   char tmpbuf[1];
   union REGS regs;
	char drivename[5];

   do 
   { /* until we get it write! */
		No_abort = ZTRUE;
      regs.h.ah = 0xd;
      int86(0x21, &regs, &regs);
      Gamechn = zopen(Gamfile, RDONLY);
      if (Gamechn < 0)
      { /* didn't work yet */
         PTVWRD(PFLAGS, GTVWRD(PFLAGS) | FSTAT);
			mcrlf();
         gamprnt("Please put ");
#if defined( SHOGUN )
			gamprnt("Shogun");
#elif defined( JOURNEY )
			gamprnt("Journey");
#elif defined( ZORK0 )
			gamprnt("Zork Zero");
#elif defined( ARTHUR )
			gamprnt("Arthur");
#endif
			gamprnt(" Disk #2 in drive ");
         if ( Swapdrive == 0 )
			{ /* no explicit drive, so use drive gamefile started in */
            drivename[0] = (unsigned char)(Cur_drive + 'A');
			}
         else
            drivename[0] = (unsigned char)(Swapdrive + 'A' - 1);

			drivename[1] = '.';
			drivename[2] = '\0';
			gamprnt( drivename );
         mcrlf();
         gamprnt("Press any key when ready, 'Q' to quit game: ");
         if ( lc(md_inp()) == 'q' )
			{ /* then just die */
				z_exit( 1 );
			}
         mcrlf();
      }   
      else
      { /* opened it, so read in 1 byte */   
         far_read(Gamechn,0,(unsigned)tmpbuf,1);
      }
   } while ( Gamechn < 0 );

	No_abort = ZFALSE;
}

/*****
* Functions added for window functionality in YZIP
*****/
/* BASE ALGORITHM :
1. Determine if # of lines specified, else default to one
2. Determine direction of scroll
3. Determine if scrolling present screen
4. Assign Present Window Pointer (pwp) to appropriate window
5. Set up registers based upon chosen window
6. Do BIOS call
*/

void scroll(id,linz)
   int id, linz;
{
#ifdef C_SCROLL
   union REGS regs;
#endif
   struct WINDOW *pwp;

   if ( id == -3 )
      id = Screen;

   pwp = &wattrib[id];

	iscroll( pwp->xpos, pwp->ypos, pwp->xsize, pwp->ysize,
   	linz, Bgcolor );

#ifdef C_SCROLL
   if ( Display & VID_MCGA )
   { /* do special mcga scroll */
      mcga_scroll( pwp->xpos, pwp->ypos+1, pwp->xsize, pwp->ysize,
         linz, Bgcolor );
      return;
   }
   if ( Display & VID_EGA )
   { /* do special ega scroll */
      ega_scroll( pwp->xpos, pwp->ypos+1, pwp->xsize, pwp->ysize,
         linz, Bgcolor );
      return;
   }

   if ( linz < 0 )
   { /* we must be scrolling down */
      regs.h.ah = 7;
      linz = -linz;
   }
   else
   { /* scroll up */
      regs.h.ah = 6;
   }
    
   regs.h.al = (unsigned char)(linz / Font_height);
	regs.h.ch = (unsigned char)(pwp->ypos / Font_height);
   regs.h.cl = (unsigned char)(pwp->xpos / Font_width);
   regs.h.dh = (unsigned char)((pwp->ypos + pwp->ysize)/Font_height) - 1;
   regs.h.dl = (unsigned char)((pwp->xpos + pwp->xsize)/Font_width) - 1;
   regs.h.bh = Bgcolor;
   int86(0x10,&regs,&regs);
#endif
}
void md_erase( pixcnt )
	int pixcnt;
{
	if ( pixcnt == 1 )
	{ /* just do a clear to EOL */
		md_clear_eol();
	}
	else
	{ /* clear from current position for this many pixels */
		clear_area( Scry, Scrx, pixcnt, Font_height );
	}
}

/* print a string using bios call */
void md_outstr( string )
	char *string;
{
	union REGS regs, cregs, xregs;

	cregs.h.ah = 3;			/* Return Cursor Status */
	cregs.h.bh = 0;			/* Video Page */
	int86( 0x10, &cregs, &cregs );

	for ( ; *string; string++ )
	{ /* just print out the string one character at a time */
		if ( *string == '\n' )
		{ /* move down a line */
			cregs.h.dh++;
			cregs.h.dl = 0;
			cregs.h.ah = 2;			/* Set Cursor Location */
			cregs.h.bh = 0;			/* Video Page */
			int86( 0x10, &cregs, &xregs );
		}
		else
		{ /* just print it out */
			regs.h.ah = 0x0A;			/* Write Character */
			regs.h.al = *string;		/* ASCII Code */
			regs.h.bh = 0;				/* Background Pixel/Video Page */
			regs.h.bl = 1;				/* Foreground */
			regs.x.cx = 1;				/* Repitition Factor */
			int86( 0x10, &regs, &xregs );

			/* and move to the left */
			cregs.h.dl++;
			cregs.h.ah = 2;			/* Set Cursor Location */
			cregs.h.bh = 0;			/* Video Page */
			int86( 0x10, &cregs, &xregs );
		}
	}
}

void dos_fatal( errnum )
	int errnum;
{
	gamprnt( "DOS read error number: " );
   print_number( errnum, md_putc );
	mcrlf();
	gamprnt("Hit [RETURN] to continue");
	md_inp();
	fatal( "DOS READ ERROR" );
}


