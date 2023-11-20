/************************************************************************
*                                                                       *
* S Y S T E M   D E P E N D E N T                                       *
*         Part I                                                        *
*                                                                       *
************************************************************************/
#include "zipdefs.h"
#include "struct.h"
#include "sysdep.h"
#include "extern.h"

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
#include <stdarg.h>
#include <time.h>
#include <dos.h>

/* globals shared by sysdeps */
extern char curs_save_top;
extern char curs_save_bot;
extern char curs_off;
extern int palette_size;
extern int wid;      /* for md_erase_char of mcga */

static unsigned char Set_palette = ZFALSE;	/* TRUE if EGA/VGA/MCGA card */
															/* so we can set up for 640x200 */
															/* CGA white on black */

void do_seek(fd, how, where)
   int fd, how, where;
{
   union REGS regs;

   regs.h.ah = 0x42;
   regs.h.al = (unsigned char)how;
   regs.x.cx = 0;
   regs.x.dx = where;
   regs.x.bx = fd;
   int86(0x21, &regs, &regs);
}

int md_mouse(fcn )
   int  fcn;
{
   union REGS regs;
   struct SREGS sregs;
	char far *mvect;

   va_list args;
   va_start( args, fcn );

   if ( !Mouseflg )
	{ /* don't bother without mouse */
      return( ZFALSE );
	}

   regs.x.ax = fcn;        /* set the register for proper call */
   switch(fcn) {           /* set the appropriate variables */
	case FIND_MOUSE:
		regs.h.ah = 0x35;		/* Get Interrupt Vector */
		regs.h.al = 0x33;		/* Interrup Number: Int 33 is mouse */
		int86( 0x21, &regs, &regs );	/* ask DOS for vector */
      sregs.es = get_segreg( ES_REG );
		if ( (regs.x.bx & sregs.es) != 0 )
		{ /* some kind of vector see what kind */
			mvect = MK_FP( sregs.es, regs.x.bx );
			if ( *mvect != (char)0xcf )
			{ /* not an IRET call, see what it does */
				regs.x.ax = 0;
				int86( 0x33, &regs, &regs );
				if ( regs.x.ax == 0xffff )
				{ /* yes, we have a mouse */
					return ZTRUE;
				}
			}
		}
		return ZFALSE;
   case INIT_MOUSE:
#ifdef MAN_MOUSE
      if (!Mouse_on)
         return(0);
#endif
      int86(0x33, &regs, &regs);
      break;
   case SHOW_MOUSE:
      if (!Mouse_shows)
         int86(0x33, &regs, &regs);
      Mouse_shows = ZTRUE;
      if ( !(Display & VID_MCGA) )
      { /* jsf 729 */
         if (!curs_off)
         { 
            regs.h.ah = 3;
            int86(0x10, &regs, &regs);
            curs_save_top = regs.h.ch;
            curs_save_bot = regs.h.cl;
            curs_off = 1;
         }
         regs.h.ah = 1;
         regs.h.ch = 0x20;
         regs.h.cl = 0;
         int86(0x10, &regs, &regs);
      }
      break;
   case HIDE_MOUSE:
      if (Mouse_shows)
      	int86(0x33, &regs, &regs); /* make the mouse call */
      Mouse_shows = ZFALSE;
      break;
   case SET_MOUSE_INPUT_MASK:
      regs.x.cx = 0x4;                /*arg1 input mask */
      regs.x.dx = (unsigned) va_arg( args, void * );   /* address offset to routine */
      sregs.cs = get_segreg( CS_REG );
      int86x(0x33, &regs, &regs, CS_REG, sregs.cs );
      break;
   case SET_MOUSE_Y_BOUNDS:
      regs.x.cx = va_arg( args, int );
      regs.x.dx = va_arg( args, int );
      int86(0x33, &regs, &regs);
      break;
   case SET_MOUSE_X_BOUNDS:
      regs.x.cx = va_arg( args, int );
      regs.x.dx = va_arg( args, int );

		if ( Display & VID_MCGA )
		{ /* double x stuff for MCGA */
			regs.x.cx *= 2;
			regs.x.dx *= 2;
		}
      int86(0x33, &regs, &regs);
      break;
   case SET_MOUSE_TEXT_CURSOR:
      regs.x.bx = va_arg( args, int );
      regs.x.cx = va_arg( args, int );
      regs.x.dx = va_arg( args, int );
      int86(0x33, &regs, &regs);
      break;
   case GET_MOUSE_POSITION:
      int86(0x33, &regs, &regs); /* make the mouse call */
      Mouse_buttons = regs.x.bx; /* bit 0 left, bit 1 right, 1=down */
      Mouse_cur_x = regs.x.cx;
      Mouse_cur_y = regs.x.dx;
		if ( Display & VID_MCGA )
		{ /* massage x pos to fit MCGA screen */
			Mouse_cur_x /= 2;
		}
      break;
   case SET_MOUSE_POSITION:
		Mouse_cur_x = va_arg( args, int );
		if ( Display & VID_MCGA )
		{ /* massage x pos to fit MCGA screen */
			Mouse_cur_x *= 2;
		}

      regs.x.cx = Mouse_cur_x;
      regs.x.dx = Mouse_cur_y = va_arg( args, int );
      int86(0x33, &regs, &regs); /* make the mouse call */
      break;
   }   
   va_end( args );
   return(regs.x.ax);
}

/* Call this with -1 to toggle the cursor, with 0 to kill it, with
   1 to turn it on (least useful operation) */
void md_write_cursor(state)
   int state;
{ 
	static int cursor_on = 0;
   char sfg;

   if ( !Do_cursor )
   { /* told not to muck with the cursor state */
      return;
   }

   if ( state == -1 || state != cursor_on )  
   { /* change the current state, please */
      if ( !cursor_on )
      { /* toggle colors then */
      	sfg = Fgcolor;
         Fgcolor = Bgcolor;
			Bgcolor = sfg;
      }

		if ( (CWIDTH( SPACE ) + Scrx) > Win_right )
		{ /* don't wrap the cursor, make it go down a line */
			mcrlf();
		}

      md_putc( SPACE );
      Scrx -= CWIDTH( SPACE );

		if ( !cursor_on )
		{ /* toggle colors back */
			Bgcolor = Fgcolor;		  
      	Fgcolor = sfg;
		}

      /* now toggle the current mode */
      cursor_on = !cursor_on;
   }
}
#ifdef DO_TANDY
int scan_copyright(ptr,str)
   unsigned char far *ptr;
   unsigned char *str;
{
   unsigned char *savstr;
   unsigned char far *savptr;
   int maxscan = 256;

   savstr = str;
   while ( maxscan-- )
   {
      if (*ptr == *str)
      {
         savptr = ptr;
         while (1)
         {
            if ( !(*++str) )
               return(1);

            if ( *++ptr != *str )
            {
                 str = savstr;
                 ptr = savptr;
                 break;
            }
         }
      }
      ptr++;
   }
   return(0);
}
#endif

void md_check_display()
{
#ifdef DO_TANDY
   unsigned char far *idpointer;
#endif

   int s1;
   union REGS regs;

/* Page 4-14 of the PS/2 & PC BIOS Technical Reference describes
   the following method for providing "video function compatibility".
   1) Do int 10H/1AH, which only works on PS/2s.  Win immediate.
   2) If that didn't work, do int 10H/12H, return EGA information,
     which only works with EGA.
   3) Perform a presence test(?) on video buffer addresses 0b8000H
     & 0b0000H to determine which functions are present. */

   regs.h.ah = 0x1A;    /* Video Display Combination */
   regs.h.al = 0;       /* Return it */
   int86(0x10, &regs, &regs);
   if ( regs.h.al == 0x1A ) 
   { /* Function supported, must have VGA Bios */
		Set_palette = ZTRUE;

      if ( Display & VID_CGA )
      { /* if user wants CGA, they can have it */
         return;
      }
      s1 = regs.h.bl;
      if (s1 & 1) 
      { /* Odd display code is mono ... */
         if ( (Display & VID_MCGA) || (Display & VID_EGA) )
         { /* picked some other mode, complain and die */
            fatal( "Incorrect video mode - must use CGA on monochrome display");
         }
         Display = VID_CGA;            /* use mono CGA mode */
      }
      else
      { /* must have a color monitor, do they want MCGA or is it MCGA? */
         if ( !(Display & VID_MCGA) && (s1 < 9) )
         { /* didn't pick/have MCGA, so use EGA */
            Display = VID_EGA;
            palette_size = 16;
         }
			else
			{ /* set Display to MCGA */
				Display = VID_MCGA;
			}
      }
      return;
   }

   if ( Display & VID_MCGA )
   { /* No VGA card, complain and die */
      fatal("Video system does not support MCGA mode!");
   }

   if ( !(Display & VID_CGA) )
   { /* didn't select CGA, so see if EGA is around */
      regs.h.ah = 0x12;    /* Video Subsystem Config */
      regs.h.bl = 0x10;    /* return it */
      int86(0x10, &regs, &regs);
      if (regs.h.bl != 0x10) 
      {  /* Have EGA */
			Set_palette = ZTRUE;
         if ( regs.h.bl == 0 )
         { /* not enough video memory for EGA - sorry */
            if ( Display & VID_EGA )
            { /* wanted EGA, but not enough memory */
               fatal("Not enough video memory for EGA!");
            }
            Display = VID_CGA;
         }
         else
         { /* show we have EGA */
            Display = VID_EGA;
         }
         return;
      }
   }
	Display = VID_CGA;
	 
#ifdef DO_TANDY
   /* if those didn't work, check for tandy */
/*   idpointer = MK_FP( 0xf000, 0xfffe );*/

   idpointer = MK_FP( 0xf000, 0xc000 );
   if (*idpointer == 0x21) 
   { /* see if tandy display */
      if (scan_copyright(idpointer, "Tandy")) 
      {
         Display = VID_TANDY;
      } 
   }
#endif
}

/* This is what the Alternate font pointed to when we started */
static int Save_af_off, Save_af_seg;

void md_init_screen()
{ 
   int jj;

   md_check_display();

#ifdef GERMAN
   language_ok = 0;
#endif

   for (jj = 0; jj < 8; jj++)
   { /* init special char for inverse video cursor */
      *(Alt_font+jj) = 0xff;
   }

   /* now set the display like we want it */
   md_set_display();
}
void md_set_display()
{
   union REGS regs;

	/* start by pretending there is color */
   Mode_bits = MODE_COLOR;

   if ( Display & VID_MCGA )
   { /* set up for 256 color, 320x200 mode */
      vb_ptr = MK_FP( 0xa000, 0 );
      regs.h.al = 0x13;
      Max_screen_width = MCGA_WIDTH;
      Ibm_to_zip = Mcga_to_zip;
      Zip_to_ibm = Zip_to_mcga;
      Zip_font_width = MCGA_FONT_WIDTH;

		switch ( FontNo )
		{ /* which font to use */
		case 1:
      	Font = MK_FP( FP_SEG( (&Font1) ), FP_OFF( (&Font1) ));
      	FontWid = &Font1Wid;
			break;
		case 2:
      	Font = MK_FP( FP_SEG( (&Font2) ), FP_OFF( (&Font2) ));
      	FontWid = &Font2Wid;
			break;
		case 3:
      	Font = MK_FP( FP_SEG( (&Font3) ), FP_OFF( (&Font3) ));
      	FontWid = &Font3Wid;
			break;
		default:
		case 0:
      	Font = MK_FP( FP_SEG( (&Font0) ), FP_OFF( (&Font0) ));
      	FontWid = &Font0Wid;
			break;
		}
   }
   else if ( Display & VID_EGA )
   { /* use 640x200, 16 color mode */
      regs.h.al = 0x0E;
      Max_screen_width = EGA_WIDTH;
      Ibm_to_zip = Ega_to_zip;
      Zip_to_ibm = Zip_to_ega;
   }
   else if ( Display & VID_CGA )
   { /* use 640x200, 2 color mode */
      regs.h.al = 0x06;
      Max_screen_width = CGA_WIDTH;
      Mode_bits &= ~MODE_COLOR;                /* no COLOR */
      Ibm_to_zip = Cga_to_zip;
      Zip_to_ibm = Zip_to_cga;
   }
#ifdef DO_TANDY
   else if ( Display & VID_TANDY )
   { /* use Tandy/Jr 640x200, 4 color mode */
      regs.h.al = 0x09;
      Max_screen_width = TANDY_WIDTH;
   }
#endif
   /* actually set the mode */
   regs.h.ah = 0;
   int86(0x10, &regs, &regs);

   /* height is always the same */
   Max_screen_height = SCREEN_HEIGHT;

   /* these modes are always on */
   Mode_bits |= MODE_DISPL|MODE_UNDE|MODE_MONO|MODE_SOUND;

   Lines = Max_screen_height / Zip_font_height;
   
#ifdef DO_FONTS
   if ( !(Display & VID_MCGA) )
   { /* Don't bother changing font for MCGA - we draw it ourselves */
      md_install_fonts();
	}
#endif

   md_init_color();

   get_disk_info();

	/* init windows again to pick up (possibly) new display data */
	wind_init();

}

void md_init_color()
{
   union REGS regs;
	if ( Display & VID_EGA )
   { /* set up new palette for 16 color ega */
      Bgcolor = DEF_EGA_BG_COLOR;
      Fgcolor = DEF_EGA_FG_COLOR;

      Ega_palette[16] = Bgcolor;    /* set up border color */
      regs.h.ah = 0x10;             /* Set Palette Registers */
      regs.h.al = 0x02;             /* Update All 16 Palette Registers + Border */
      regs.x.dx = (unsigned)Ega_palette; /* Address of 17-byte table */
      int86x( 0x10, &regs, &regs, ES_REG, get_segreg( DS_REG ) );
   }
#ifdef DO_TANDY
   else if ( Display & VID_TANDY )
   { /* set up special Tandy mode */
      Bgcolor = DEF_TANDY_BG_COLOR;
      Fgcolor = DEF_TANDY_FG_COLOR;

      Tandy_palette[0] = Bgcolor;
      Tandy_palette[1] = 0xff;
      Tandy_palette[2] = 0xff;
      Tandy_palette[3] = Fgcolor;

      /* and set color 3 to be the foreground */
      regs.h.ah = 0x10;          /* Set Palette Registers */
      regs.h.al = 0;             /* Update Palette Registers */
      regs.h.bh = Fgcolor;       /* Color Value */
      regs.h.bl = 3;             /* Palette Register */
      int86(0x10, &regs, &regs); 
   }
#endif
   else if ( Display & VID_MCGA )
   { /* initialize palette for MCGA mode */
      Bgcolor = DEF_MCGA_BG_COLOR;
      Fgcolor = DEF_MCGA_FG_COLOR;

      regs.h.ah = 0x10;             /* Set Palette Registers */
      regs.h.al = 0x12;             /* Update Block of DAC Registers */
      regs.x.bx = 16;               /* First Register */
      regs.x.cx = 16;               /* Number of Registers */
      regs.x.dx = (unsigned)Mcga_palette; /* Address of Table */
      int86x(0x10, &regs, &regs, ES_REG, get_segreg( DS_REG ) );
   }
   else if ( Display & VID_CGA )
   { /* just set up foreground color for CGA */
      Bgcolor = DEF_CGA_BG_COLOR;
      Fgcolor = DEF_CGA_FG_COLOR;

		if ( Set_palette )
		{ /* set up the palette for white on black */
      	regs.h.ah = 0x0B;             /* Set Overscan Color */
      	regs.h.bh = 0;                /* Set Border Color */
      	regs.h.bl = Bgcolor;          /* Color Value */
      	int86(0x10, &regs, &regs);    /* Set BG for CGA */
	
			set_dacreg( 0, 2, Mcga_palette );
#ifdef NNN
      	regs.h.ah = 0x10;             /* Set Palette Registers */
      	regs.h.al = 0x00;					/* Update Specific Register */
      	regs.h.bh = 0x17;					/* set foreground register to white */
			regs.h.bl = 1;						/* register 1 */
      	int86x( 0x10, &regs, &regs, ES_REG, get_segreg( DS_REG ) );
#endif
		}
   }

   Def_fgcolor = Fgcolor;           /* set up default colors */
   Def_bgcolor = Bgcolor;
}

void md_install_fonts()
{
   int far *chrvptr;
   union REGS regs;   
   unsigned save_es;

   /* install YZIP alternate font address */
   chrvptr = MK_FP( 0, 0x007C );
   Save_af_off = *chrvptr;        /* save current alt font, offset */
   FP_OFF(chrvptr) = 0x7e;
   Save_af_seg = *chrvptr;        /* current alt font, segment */

   /* install font for chars 0x80 to 0xFF */
   zusrfont( Alt_font );

   /* go get the main font address */
   regs.h.ah = 0x11;       /* Character Generator Interface */
   regs.h.al = 0x30;       /* Get Current Character Generator Info */
   regs.h.bh = 1;          /* Contents of Int 0x43 */
   int86(0x10, &regs, &regs);

   /* go get that font and copy it into my area */
   save_es = get_segreg( ES_REG );
   chrvptr = MK_FP( 0, 0x010C );
   do_copy(save_es, *chrvptr, get_segreg( DS_REG ),
      (unsigned)&Main_font[0], 128*regs.x.cx);
   zdumpfont(&Main_font[0]);        /* install YZIP main font */
}

void md_ttyres()
{
   int far *chrvptr;
   union REGS regs;   

   if ( !(Display & VID_MCGA) )
   { /* if not MCGA, we trashed the character set */
      chrvptr = MK_FP( 0, 0x007C );
      *chrvptr = Save_af_off;
      FP_OFF(chrvptr) = 0x7e;
      *chrvptr = Save_af_seg;
   }

   if ( Mouseflg )
   { /* reset the mouse */
      Mouseflg = ZFALSE;
      regs.x.ax = 0;
      int86(0x33,&regs, &regs);
      if ( curs_off )
      {
         regs.h.ah = 1;
         regs.h.ch = curs_save_top;
         regs.h.cl = curs_save_bot;
         int86(0x10, &regs, &regs);
      }
   }

   regs.h.ah = 0;
   regs.h.al = Oldmode;
   int86(0x10, &regs, &regs);
}


int md_getch()
{
   union REGS regs;

   regs.h.ah = 0;
   int86(0x16, &regs, &regs);
   return(regs.x.ax);
}

static unsigned int rseed1;
static unsigned int rseed2;

unsigned char md_cvtchr(chr)
   int chr;
{
   unsigned char val;
   unsigned char lowch = (unsigned char)chr & 0xff;   /* get low byte */

   if (lowch != 0)
   {   /* normal key */
      if (lowch >= ' ')
      {
         if (lowch < 127) 
            val = lowch;
         else
            val = '\b';
      }
      else if ((lowch == '\r') || (lowch == '\n')) 
         val = '\r';
      else if (lowch == '\b') 
         val = lowch;
      else if (lowch == 3) 
         z_exit(2);
      else
         val = 0;
   }
   else
	{ /* extended key */
      switch (chr >> 8)
		{	
      case 72:
         val = UP_ARROW;
         break;
      case 80:
         val = DOWN_ARROW;
         break;
      case 75:
         val = LEFT_ARROW;
         break;
      case 77:
         val = RIGHT_ARROW;
         break;
      case 59: case 60: case 61: case 62: case 63: case 64: case 65:
      case 66: case 67: case 68:
         val = (unsigned char)((chr >> 8) + 74);
         break;      /* f1 through f10 */
      /* others later */
      default:
         val = 0;   /* lower in ezip */
		}
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
	long click_cnt = 0;
   int chr;

	Linecnt = 0;
   md_inittime();            /* initialize */
   md_write_cursor(1);      /* turn cursor on */
	if ( Mouseflg )
		md_mouse( SHOW_MOUSE );

	while( ZTRUE )
   { /* just break out when done */
      if ( Mouse_event_count > 0 )
      { /* must be an event */
    		if ((chr = do_mouse_event(&Mouse_frob[0])) != 0)
         { /* button click, is it a double? */
				if ( click_cnt == 0 )
				{ /* first click, start counter */
					click_cnt = get_time();
				}
				else
				{ /* must be a double click then */
					chr = DOUBLE_CLICK;
					break;
				}
         }
      }
		if ( click_cnt != 0 )
		{ /* check for double click time out */
			if ( (get_time() - click_cnt) > DOUBLE_CLICK_COUNT )
			{ /* just a single click, then */
				chr = SINGLE_CLICK;
				break;
			}
		}
      if ( kbhit() )
      { /* key has been hit, see what it was */
   		if ((chr = md_cvtchr(md_getch())) != 0)
         { /* good character found */
				break;
         }
			else
			{ /* complain */
				md_sound(FEEP);
			}
      }
      elapsed = md_timed_out();   /* returns elapsed time in 100ths */
      if ((elapsed - old_elapsed) >= CURSOR_BLINK)
      { /* make cursor blink then */
         if (time == 0)
         { /* reset timer */
            old_elapsed = 0;
            md_inittime();
         }
         else
         {
            old_elapsed = elapsed;
         }
         md_write_cursor(-1);
      }
      if (time && ((elapsed / 10) > time))
      { /* we've timed out, so check up on function */
         if (internal_call(handler))
         {   /* abort requested, so kill read */
				chr = 0;
				break;
         }
			else
			{ /* reset timer */
         	old_elapsed = 0;
         	md_inittime();      /* start timeout over */
			}
      }
	}
	md_write_cursor(0);   /* make sure cursor off */
	if ( Mouseflg )
   	md_mouse( HIDE_MOUSE );

   return(chr);
}

void md_hlite(attrib)
   ZIPINT attrib;
{ 
   if (attrib == 0)  
   { /* reset all attributes */
      if ( Win_hlmode & REVERSE )
      { /* swap colors back */
         attrib = Fgcolor;
         Fgcolor = Bgcolor;
         Bgcolor = attrib;
      }
      Win_hlmode = 0;
   }
   else 
   {
      Win_hlmode = Win_hlmode | attrib;

      if (attrib & REVERSE)
      { /* Reverse Video, swap background and foreground */
         attrib = Fgcolor;
         Fgcolor = Bgcolor;
         Bgcolor = attrib;
      }
   }
}

/* Get old display, and pretend we are an EGA display for now */
void md_init_stuff()
{
   union REGS regs;

   /* check to see what the current mode is, so we can go back to it */
   regs.h.ah = 0x0f;       /* Get Current Video Status */
   int86(0x10, &regs, &regs);
   Oldmode = regs.h.al;

   Font_width = Zip_font_width = FONT_WIDTH;
   Font_height = Zip_font_height = FONT_HEIGHT;
}

void md_setup()
{  /*    Setup performs any system dependent initialization that must be
   done only once.
   */
   int crterr();
   union REGS regs;
   struct SREGS sregs;

   md_init_stuff();        /* initialize a few things first */
   /* install error handler */
   regs.x.dx = (unsigned)crterr;
   regs.h.ah = 0x25;
   regs.h.al = 0x24;
   sregs.cs = get_segreg( CS_REG );
   int86x(0x21, &regs, &regs, DS_REG, sregs.cs );
   /* find out what kind of display we are running on */
   md_init_screen();

   wind_init();
}

void mtime()
{  /* mtime get the machine time for setting the random seed.
   */
   union REGS regs;

   regs.h.ah = 0x2c;
   int86(0x21, &regs, &regs);   /* time of day */
   if (regs.x.cx == 0)
   { /* didn't really work, so make something up */
      rseed1 = 0x8d0e;
      rseed2 = 0x9f81;
   }
   else
   {
      rseed1 = regs.x.cx;
      rseed2 = regs.x.dx;
   }
}

int rand()
{
   unsigned int temp;
   int flg = rseed1 & 1;

   temp = rseed1;
   rseed1 = rseed1 >> 1;
   rseed2 = rseed2 >> 1;
   if (flg)
      rseed2 = rseed2 | 0x8000;
   rseed1 ^= rseed2;
   rseed2 = temp;
   return(rseed1 & 0x7fff);
}

void md_clr( scrno )
   int scrno;
{
   int cur_window = ZFALSE;
   struct WINDOW *pwp;
	int savebg;

   /* this used to be done w/ curr._highlite in OPCLEAR */   
   wattrib[scrno].hlmode = 0;   
   if ( ( scrno == -3) || (scrno == Screen) )
   { /* doing current screen, so say so and copy globals */
      cur_window = ZTRUE;
      scrno = Screen;
      window_to_globals( scrno );
   }

   pwp = &wattrib[scrno];

   home( pwp );                      /* home the cursor */

	savebg = Bgcolor;
	Bgcolor = pwp->bgcolor;

   clear_area( pwp->ypos, pwp->xpos, pwp->xsize, pwp->ysize );

	Bgcolor = savebg;

   if (cur_window)
   { /* if in current window, move cursor and update globals */
      window_to_globals( scrno );
   }
}

/* Just use character based clear for now */
void clear_area( top, left, width, height )
   int top, left, width, height;
{
	iscroll( left, top, width, height, height, Bgcolor );
}

/* just set Scrx/Scry/CharX and quit */
void locate(row, col)
   short row,col;
{
   Scrx = CharX = col;
   Scry = row;
}


/* this does cr without scripting.  Everybody except routines that explicitly
   don't want scripting should call mcrlf, which does scripting. */
void md_mcrlf()
{  /* machine dependent (actually vt100) method for doing windowed scrolling.
   */
   int xsize;

   if ( !Vidflg )
   { /* no video! */
      return;
   }

   xsize = Win_xsize;
   if (Win_attributes & SCROLLMASK)
   { /* do work for screen x */
      if ( (Scry+Font_height) > (Win_ypos+Win_ysize-Font_height))
      { /* at bottom of screen, so do a scroll */
			iscroll( Win_xpos, Win_ypos, xsize, Win_ysize,
          	Font_height, Bgcolor );
         locate( Scry, (Win_xpos+Win_lmarg) );
      }   
      else
      { /* no scroll, just move down one line */
         locate((Scry+Font_height),(Win_xpos + Win_lmarg));
      }   
      Linecnt++;         /* more counter */
      if (Linecnt >= Lines-1 )  
      { /* too many lines since last input, ask for [MORE] */
         md_printstr("[MORE]");
         md_tinp(0,0);

         locate(Scry,(Win_xpos+Win_lmarg));
			md_printstr("        ");			/* erase the more */
         locate(Scry,(Win_xpos+Win_lmarg));
         Linecnt = 0;
      }
   }   
   else
   { /* handle non-scrolling windows */
      if ( (Scry + Font_height) <= (Win_ypos+Win_ysize-Font_height))
      { /* just move down one line if not going off the screen */
         locate((Scry+Font_height),(Win_xpos+Win_lmarg));
      }
      else
      { /* just go back to beginning of line */
         locate(Scry,(Win_xpos+Win_lmarg));
      }

      if (Win_attributes & WRAPMASK)
         md_clear_eol();
   }
}

void mcrlf()
{
   if (Scripting && (Win_attributes & SCRIPTMASK))
      md_script_cr();

   md_mcrlf();

	/* make sure CharX gets reset */
	if (Vidflg || FTblout)
		/* Only if we're actually displaying stuff */
		CharX = Win_xpos + Win_lmarg;
}

void md_clear_eol()
{
   clear_area( Scry, Scrx, Win_xsize+Win_xpos-Scrx-Win_rmarg, Font_height );
}

void md_printstr(str)
   char *str;
{ 
	while ( *str )
   { /* print that string */
		md_putc( *str++ );
   }
}


static long inittime;      /* in 100ths */

long get_time()
{
/* 100ths since midnight */
   union REGS regs;

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
{
   long ctime = get_time();

   if (itime > ctime)
   { /* must have gone past midnight */
      ctime = itime - ctime;
   }   
   else
      ctime = ctime - itime;
   return((short)(ctime & 0xffff));
}

void md_wait(secs)
int secs;
{
   int lsecs = secs * 100;
   long itime = get_time();

   while (1)
   {   
      if (md_time_diff(itime) >= lsecs)
         break;
   }   
}

int md_timed_out()
{
   return(md_time_diff(inittime));
}

int md_getl(buf, cnt, i, timeout, timer)
   char *buf;
   int cnt;
   int i;
   int timeout;
	ZIPINT timer;
{  /*   Machine (or OS) dependent line read.  Md_getl reads chars up to cnt.
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
   char letter;

   Linecnt = 0;            /* zero line counter now */

   for (read_terminator = 0; read_terminator == 0; )
   { /* until we get a read terminator (or maybe we'll timeout first) */
		c = md_tinp( timeout, timer );
		if ( c == 0 )
		{ /* timed out and died, so return current count */
			return( -1-i );
		}
      /* Now have a character in c, presumably structured as in technical manual */
      if ((c != EOL) && (c != CR) && not_terminator(((unsigned char)c)))
      { /* a real character */
         if (c != BKSPC)
         { /* not a backspace character */
            if ( isprint(c) )
            { /* printable */
               if (i < cnt && ((CWIDTH(c)+Scrx) < (Win_right-CWIDTH(SPACE))) )
               { /* there's still room for the cursor */
                  *(buf + i) = (unsigned char)c;      /* printable chars get saved... */
                  i++;
#ifdef GERMAN
                  cvt_putc(c);
#else
                  md_putc((char)c);      /* and echoed, of course */
#endif
               }
               else 
               { /* there is no room */
                  md_sound(FEEP);
               }
            }
            else if (c != -1) 
               md_sound(FEEP);
         }   
         else if (i)
         { /* handle a backspace */
            i--;
            if ( Display & VID_MCGA )
            { /* do variable width font */
               letter = *(buf + i);
               wid = CWIDTH( letter );
            }
            *(buf + i) = NULL;   /* wipe out last char in buffer */
            md_erase_char();      /* rubout the last char typed */
         }
         else
         { /* nothing to backspace over */
            md_sound(FEEP);      /* left margin, so beep */
         }
      }   
      else
      { /* got the line */   
         read_terminator = (char)c;
      }
   }   
   *(buf + i) = NULL;         /* make an end string */
   return(i);
}
char do_mouse_event( tab )
   int *tab;
{
   ZIPINT extab;

   if ( get_mouse_event(tab) )
   { /* there sure was a mouse click */
      if (((extab = GTVWRD(PEXTAB)) != 0) && (GTVWRD(extab) > 1))
      { /* there is a table, so show where it happened */
			if ( Display & VID_MCGA )
			{ /* divide x by two, cuz mcga is 1/2 as big */
				tab[1] >>= 1;
			}

         PTVWRD(extab + (2 * PMSLOCX), tab[1] + 1);
         PTVWRD(extab + (2 * PMSLOCY), tab[2] + 1);
         return(253);
      }   
   }   
   return(0);
}
