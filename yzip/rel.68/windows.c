/************************************************************************
*                                                                       *
*  W I N D O W  R O U T I N E S                                         *
*                                                                       *
*************************************************************************/
#include <dos.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"
#include "sysdep.h"

void valid_curs( wptr )
   struct WINDOW *wptr;
{

   if( wptr->ycurpos >= wptr->ysize )
   { /* too far gone, move it to top */
      wptr->ycurpos = wptr->linecnt = 0;
   }

   if ( wptr->xcurpos >= (wptr->xsize-wptr->rmarg-wptr->lmarg) ||
		  wptr->xcurpos < wptr->lmarg )
   { /* too far left/right, move to left edge */
      wptr->xcurpos = wptr->lmarg;
   }
}
/* clear the entire screen, and home the cursors */
void clear_screen()
{
   int num;
   union REGS regs;

	globals_to_window( Screen );

   clear_area( 0, 0, Max_screen_width, Max_screen_height );

	if ( Display & VID_EGA )
	{ /* do the border color too */
      regs.h.ah = 0x10;             /* Set Palette Registers */
      regs.h.al = 0x01;					/* Set Border Color */
		regs.h.bh = Bgcolor;				/* Color Value */
      int86x( 0x10, &regs, &regs, ES_REG, get_segreg( DS_REG ) );
	}

   for (num = 0; num < 8; num++)
   { /* do home for each window */
      home( &wattrib[num] );
   }

	window_to_globals( Screen );
}
/* move cursor to top left of selected window */
void home( wptr )
   struct WINDOW *wptr;
{
	wptr->xcurpos = wptr->lmarg;
	wptr->ycurpos = wptr->linecnt = 0;
}


/* BASE ALGORITHM:
1. Determine if working on current window
2. Assign pwp pointer
3. Determine if window will go beyond screen's limits
4. If it does, scale down the window
5. If working on current window, reposition the window
*/
void winpos( id, y, x)
   int id,y,x;             /* position of window based */
{                          /* upon upper r.h. corner */
   int cur_window = ZFALSE;
   struct WINDOW *pwp;

   if((id == -3) || (Screen == id))
   { /* mucking with current screen */
      globals_to_window(Screen);
      id = Screen;
      cur_window = ZTRUE;
   }
   
   pwp = &wattrib[id];

   /* make it zero relative */
   y--;
   x--;

   if ( y > Max_screen_height )
   { /* we are going too far, so clip the window */
      pwp->ysize = Max_screen_height;
   }

   if ( x > Max_screen_width )
   { /* too wide, so clip the window */
      pwp->xsize = Max_screen_width;
   }
   
   pwp->xpos = x;
   pwp->ypos = y;
#ifdef DO_EGA    
	if ( Display & (VID_EGA|VID_CGA) )
	{ /* round for EGA */
		pwp->ypos = (pwp->ypos + 4) & ~7;
		pwp->xpos = (pwp->xpos + 4) & ~7;
	}
#endif

   if ( id == Mouse_window )
   { /* mouse limits might change */
      mouse_lim( id );
   }

   valid_curs( pwp );          /* make sure cursor is still valid */

   if ( cur_window )
   { /* if working on current screen, update globals */
      window_to_globals(Screen);
   }
}

/* BASE ALGORITHM: winsize()
1. Determine if working on current screen
2. Make sure window is in boundary of screen
3. Adjust cursor position if necessary
4. If working on current screen, move the cursor
*/
void winsize(id,height,width)
   int id,height,width;
{
   int cur_window = ZFALSE;
   struct WINDOW *pwp;

   if ( id == -3 || id == Screen)
   { /* working on current window */
      globals_to_window(Screen);
      id = Screen;
      cur_window = ZTRUE;
   }

   pwp = &wattrib[id];

   if ( height > Max_screen_height )
   { /* it's too big, use max */
      height = Max_screen_height;
   }

   pwp->ysize = height;
#ifdef DO_EGA
	if ( Display & (VID_EGA|VID_CGA) )
	{ /* do special round for ega */
		pwp->ysize = (pwp->ysize + 4) & ~7;
		pwp->xsize = (pwp->xsize + 4) & ~7;
	}
#endif

   if ( width > Max_screen_width )
   { /* ditto */
      pwp->xsize = Max_screen_width;
   }
                   
   pwp->xsize = width;

   valid_curs( pwp );

   if ( id == Mouse_window )
   { /* mouse limits might change */
      mouse_lim( id );
   }

   if ( cur_window )
   { /* do some special stuff if in current window */
      window_to_globals(Screen);
   }
}
/* BASE ALGORITHM: WINATTR
1. Determine if default operation applies
2. Determine if working on current screen
3. Assign pwp
4. Decide what to do based upon value of operation
*/
void winattr(id, bits, operation)
   int id, bits, operation;         /* bit 0 -> bit 3 designate */
{                                    /* wrapping, scrolling, Scripting */
   int temp;
   int cur_window = ZFALSE;
   struct WINDOW *pwp;

   if (operation > 3 || operation < 0)
      operation = 0;
    
   if (id == -3 || id == Screen )
   { /* doing current window, so pick up all the globals */
      globals_to_window(Screen);
      id = Screen;
      cur_window = ZTRUE;
   }
   
   pwp = &wattrib[id];
    
   switch (operation) 
   {
   case 0:               /* MOV */
       pwp->attributes = bits;
       break;
   case 1:               /* SET */
       pwp->attributes |= bits;
       break;
   case 2:               /* CLEAR */
       temp = bits & pwp->attributes;
       pwp->attributes ^= temp;
       break;
   case 3:               /* COMP */
       pwp->attributes ^= bits;
       break;
   }

   if (cur_window)
   { /* copy current stuff back to globals */
      window_to_globals(Screen);
   }
}

/* BASE ALGORITHM: CURSET
1. Determine if working on current screen
2. Assign pointer to appropriate window
3. Constrain cursor position by window's dimensions 
4. Update window structure
*/

void curset(y,x,id)
int y,x,id;
{
   int cur_window = ZFALSE;
   struct WINDOW *pwp;

   if ( y < 0 )
   { /* special case - turn off (-1) / on (-2) the cursor */
      if ( y == -1 )
      { /* turn it off */
         Do_cursor = ZFALSE;
      }
      else
      { /* must be turn on */
         Do_cursor = ZTRUE;
      }
      return;
   }

   if ( (id == Screen) || (id == -3) )
   { /* reset window vars, and show we are in current screen */
      globals_to_window(Screen);
      id = Screen;
      cur_window = ZTRUE;
   }      
   pwp = &wattrib[id];
   y--;      /* zero based system from one based system */
   x--;

   /* update wattrib structure */    
   pwp->ycurpos = y;
   pwp->xcurpos = x;

   valid_curs( pwp );

   if ( cur_window )
   { /* reset the globals then */
      window_to_globals(Screen);
   }
}
/* BASE ALGORITHM: WINGET
1. Establish a pointer to first entry in window structure
2. If no offset exists, offset = 0
3. Fetch the desired data 
*/
int winget(id,offset)
   int id,offset;
{
   int val;         /* value to be returned */
   struct WINDOW *pwp;

   if ( id == -3 || id == Screen )
	{ /* set id and get globals */
      id = Screen;
   	globals_to_window( Screen );
	}

   pwp = &wattrib[id];
   switch( offset )
   {
   case 0:
      val = pwp->ypos + 1;
      break;
   case 1:
      val = pwp->xpos + 1;
      break;
   case 2:
      val = pwp->ysize;
      break;
   case 3:
      val = pwp->xsize;
      break;
   case 4:
      val = pwp->ycurpos + 1;
      break;
   case 5:
      val = pwp->xcurpos + 1;
      break;
   case 6:
      val = pwp->lmarg;
      break;
   case 7:
      val = pwp->rmarg;
      break;
   case 8:
      val = pwp->crintfcn;
      break;
   case 9:
      val = pwp->crintctr;
      break;
   case 10:
      val = pwp->hlmode;
      break;
   case 11:
      val = (pwp->zipbg << 8) | pwp->zipfg;
      break;
   case 12:
      val = pwp->fontid;
      break;
   case 13:
      val = (pwp->fontsize.height << 8) | pwp->fontsize.width;
      break;
   case 14:
      val = pwp->attributes;
      break;
	case 15:
		val = pwp->linecnt;
		break;
   }

   return(val);
}


/* BASE ALGORITHM: winput
1. Establish a pointer to first entry in window structure
2. If no offset exists, offset = 0
3. Get number of slots available in table
4. LOOP
*/
void winput( id, which1, newval )
   int newval, which1;
   int id;
{
   struct WINDOW *pwp;

   if ( id == -3 || id == Screen )
	{ /* set id, and copy globals */
      id = Screen;
		globals_to_window( Screen );
	}

   pwp = &wattrib[id];

   switch (which1)
   {
	case 8:
      pwp->crintfcn = newval;
   	break;
	case 9:
      pwp->crintctr = newval;
   	break;
	case 15:
		pwp->linecnt = newval;
		break;
   }

	if ( id == Screen )
		window_to_globals( Screen );

}                  
    

void mouse_info(tabl)
   ZIPINT tabl;
{
   int temp;

   md_mouse(GET_MOUSE_POSITION);      /* set up identifiers */
   PTVWRD(tabl,Mouse_cur_x);         /* place identifiers in specified table */
   PTVWRD(tabl+2,Mouse_cur_y);
   temp = Mouse_buttons & 3;         /* mask out all but bits 0 and 1 */

   if ( temp && temp != 3 )         /* if temp exists */
      temp ^= 3;         /* switch bits to match the spec. */

   PTVWRD(tabl+4,temp);         /* place this in table */
}    

void mouse_lim(id)
   int id;
{
   int up,left;
   short down,right;
   struct WINDOW *pwp;

   if (id == -3)
      id = Screen;

   if (id == -1)
   { /* clear mouse limits */
      up = left = 0;
      left = 0;
      down = Max_screen_height;
      right = Max_screen_width;
   }
   else
   { /* set mouse limits to this window */
      pwp = &wattrib[id];
      left = pwp->xpos;
      right = pwp->xpos + pwp->xsize;
      up = pwp->ypos;
      down = pwp->ypos + pwp->ysize;
   }

   Mouse_window = id;
   md_mouse(SET_MOUSE_Y_BOUNDS,up,down);
   md_mouse(SET_MOUSE_X_BOUNDS,left,right);
}


void domargin(lmarg,rmarg,id)
   int lmarg,rmarg,id;
{
   int cur_window = ZFALSE;
   struct WINDOW *pwp;

   if ((id == -3) || (id == Screen))
   { /* default id to current scn. */
      globals_to_window(Screen);
      id = Screen;      /* you are dealing w/ current screen */
      cur_window = ZTRUE;
   }

   pwp = &wattrib[id];      /* assign pwp to present window based on id */

   if (!(pwp->attributes & 1))
      return;   /* no-op for non-wrapping windows */

   /* note that there is no check for the size of the margins relative to */
   /* the width of the window as this routine may be called before the */
   /* size of the window is specified.  if the margins are too great, */
   /* nothing will get written to the window */

   pwp->lmarg = lmarg;
   pwp->rmarg = rmarg;

   valid_curs( pwp );

   if ( cur_window )
   { /* we did the current screen, so copy globals */
      window_to_globals(Screen);
   }
}

void globals_to_window(id)
   int id;
{
   wattrib[id].ypos = Win_ypos;
   wattrib[id].xpos = Win_xpos;
   wattrib[id].ysize = Win_ysize;
   wattrib[id].xsize = Win_xsize;
   wattrib[id].ycurpos = Scry - wattrib[id].ypos;
   wattrib[id].xcurpos = Scrx - wattrib[id].xpos;
   wattrib[id].lmarg = Win_lmarg;
   wattrib[id].rmarg = Win_rmarg;
   wattrib[id].hlmode = Win_hlmode;
   wattrib[id].fontid = Scrfont;
   wattrib[id].fontsize.height = Zip_font_height;
   wattrib[id].fontsize.width = Zip_font_width;
   wattrib[id].attributes = Win_attributes;
   wattrib[id].fgcolor = Fgcolor;
   wattrib[id].bgcolor = Bgcolor;
   wattrib[id].linecnt = Linecnt;
}
   
void window_to_globals(id)
   int id;
{
   Win_ypos = wattrib[id].ypos;
   Win_xpos = wattrib[id].xpos;
   Win_ysize = wattrib[id].ysize;
   Win_xsize = wattrib[id].xsize;

   Scrx = wattrib[Screen].xcurpos + wattrib[Screen].xpos;
   Scry = wattrib[Screen].ycurpos + wattrib[Screen].ypos;
   locate( Scry, Scrx );

   Win_lmarg = wattrib[id].lmarg;
   Win_rmarg = wattrib[id].rmarg;
   Win_hlmode = wattrib[id].hlmode;
   Scrfont = wattrib[id].fontid;
	if ( Scrfont == 4 )
	{ /* mono font, so set mono font highlight */
		Win_hlmode |= MONO_SPACE;
	}
	else
	{ /* no monospace, so clear it */
		Win_hlmode &= ~MONO_SPACE;
	}

   Font_width = wattrib[id].fontsize.width;
   Font_height = wattrib[id].fontsize.height;
   Lines = wattrib[id].ysize / Font_height;
   Linecnt = wattrib[id].linecnt;
   Win_attributes = wattrib[id].attributes;
   Fgcolor = wattrib[id].fgcolor;
   Bgcolor = wattrib[id].bgcolor;

   Scrbtm = (Win_ypos/Font_height) + Lines;
   Win_right = Win_xpos + Win_xsize - Win_rmarg;
}

void wind_init()
{
   int x;

   for ( x=0; x <= 7; x++)
   { /* init some special things */
		wattrib[x].ypos = wattrib[x].xpos = 0;
		wattrib[x].ysize = wattrib[x].xsize = 0;
		wattrib[x].ycurpos = wattrib[x].xcurpos = 0;
		wattrib[x].lmarg = wattrib[x].rmarg = 0;
		wattrib[x].crintfcn = wattrib[x].crintctr = 0;
		wattrib[x].hlmode = wattrib[x].linecnt = 0;

      wattrib[x].fontid = 1;
      wattrib[x].fontsize.height = Zip_font_height;
      wattrib[x].fontsize.width = Zip_font_height;

      wattrib[x].attributes = BUFFERMASK;   /* buffering initially on */

      wattrib[x].fgcolor = Def_fgcolor;
      wattrib[x].bgcolor = Def_bgcolor;
      wattrib[x].zipfg = Ibm_to_zip[Def_fgcolor];
      wattrib[x].zipbg = Ibm_to_zip[Def_bgcolor];
   }
   wattrib[0].ysize = Max_screen_height;
   wattrib[0].xsize = Max_screen_width;
   wattrib[0].attributes = BUFFERMASK|SCROLLMASK|SCRIPTMASK|WRAPMASK;

   wattrib[1].xsize = Max_screen_width;

#ifdef GERMAN
	wattrib[1].ysize = Font_height;
	wattrib[0].ysize = Max_screen_height - Font_height;
	wattrib[0].ypos = Font_height;
#endif

   window_to_globals(0);   /* initialize globals to window 0 values */
}

void dosplit( lines )
   int lines;
{
   flush_buffer();
   globals_to_window( Screen );

   wattrib[1].ysize = lines;
   wattrib[1].ypos = 0;          /* make top goto zero */
   wattrib[0].ypos = lines;

   if ( lines == 0 )
   { /* set to maximum & select window 0 */
      wattrib[0].ysize = Max_screen_height;
      Screen = 0;
   }
   else
   { /* make it reach the bottom of the screen */
      wattrib[0].ysize = Max_screen_height - lines;
   }

   valid_curs( &wattrib[0] );
   valid_curs( &wattrib[1] );

   window_to_globals(Screen);
}
/* OPSCREEN */
void newdoscreen(id)
int id;
{
   flush_buffer();

   /* save current globals */
   globals_to_window(Screen);
   Screen = id;            /* this is where 'screen' gets updated */
   window_to_globals(Screen);

   if (!(Display & VID_MCGA) )
       md_set_font( Scrfont, Screen );
}


