/************************************************************************
*                                                                       *
*  O U T P U T  R O U T I N E S                                         *
*                                                                       *
************************************************************************/

#include <stddef.h>
#include <dos.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"
#include "sysdep.h"

#ifdef GERMAN
#include "german.h"
#endif
static char *tailptr;

/************************************************************************
*                                                                       *
*   S T R I N G   O U T P U T   S U P P O R T                            *
*                                                                        *
************************************************************************/

#define RESET_CS  tempcs = permcs   /* reset the character set */

/* DECODE A ZSTRING */
void putstr()
{
   /*   String addr given in zblk and zoff, return updated address.

   To understand fully what this procedure does, you must be familiar 
   with zstrings as presented in the ZIP document.  Briefly, a word 
   contains 3 5-bit characters.  There are three character sets, shift 
   chars, and a funny thing called fwords.  Fwords, indicated by the 
   value three (3), are an offset into the table pointed to by wrdtab,
   which contains a pointer to another zstring.  Fword stands for
   Frequently used word.  So this routine is re-entrant.  It is
   guaranteed that an fword will not contain another fword.
   */
   ZIPINT word, bytsav, rword;
   short i, tempcs = 0, permcs = 0;
   char word_mode = ZFALSE, asciflg = 0;
   ZIPINT blk = zblk;
   ZIPINT off = zoff;
   
   do 
   {
      rword = purwrd(blk,off);      /* get a word, nothing advances */
      off += 2;                     /* advance pointer */
      for (i = 1; i <= 3; i++) 
      { /* max 3 letters in word */
         word = (rword >> (ZCHRLEN * (3 - i))) & ZCHRMSK;
         if ( !word_mode )
         {
            if ((word < 6) && (asciflg == 0))  
            { /* special common character? */
               switch (word) 
               { /* do special case for each */
               case 0: 
                  putchr(SPACE);       /* print a space char */
                  RESET_CS;
                  break;
               case 1:
               case 2:                 /* calculate fword offset */
               case 3: 
                  word--;              /* use as a multiplicand */
                  word <<= 6;          /* blocks are 64 bytes (or 32 words) */
                  wrdoff = word;       /* setup offset */
                  word_mode = ZTRUE;   /* indicate word mode */
                  break;
               case 4:
               case 5: 
                  switch (tempcs) 
                  {   /* change cs according to current */
                  case 0: 
                     tempcs = word - 3;      /* tempcs becomes 1 or 2 */
                     break;                  /* go back to the top */
                  case 1:                    /* cs's 1 + 2 both set permcs */
                  case 2: 
                     if ( tempcs != word - 3 )  /* not current cs */
                        tempcs = 0;             /* reset to 0 */
                     permcs = tempcs;           /* perm too */
                     break;
                  default:
                     fatal("Undefined shift state - putstr");
                  }
                  break;
               default:
                  fatal("Illegal special code - putstr");
               }
            }
            else
            { /* word is not special char */
               switch(tempcs) 
               {   /* so process it according to char set */
               case 0:
               case 1: 
                  putchr(getzchr(word, tempcs));   /* translate char in word */
                  RESET_CS;
                  break;
               case 2:  /* see if it is ascii special */
                  if (asciflg)  
                  { /* doing ascii escape char */
                     asciflg++;         /* byte count */
                     if (asciflg == 2)  
                        bytsav = word << ZCHRLEN;
                     else 
                     { /* pick up saved part and print entire byte */
                        word |= bytsav;      /* or in high byte */
                        putchr(word);
                        asciflg = 0;      /* reset ascii mode flag */
                        RESET_CS;
                     }
                  }
                  else            
                  { /* process non-ascii escape */
                     switch (word) 
                     {   /* cs 2 has specials for 6 & 7 */
                     case 6:           /* do ASCII escape */
                        asciflg = 1;
                        break;
                     case 7:           /* do CR/LF */
                        newlin();
                        RESET_CS;
                        break;
                     default: 
                        putchr(getzchr(word, tempcs));
                        RESET_CS;
                        break;
                     }
                  }
               }
            }
         }
         else 
         { /* we're in WORD mode */
            word <<= 1;                /* multiply it by 2 */
            word += wrdtab + wrdoff;   /* index into fword tbl */
            bsplit(GTVWRD(word));      /* get new ptr */
            putstr();                  /* and print it recursively */
            word_mode = ZFALSE;        /* turn off word mode */
            RESET_CS;
         }
      }
   } while ((rword & BIT16) == 0);   /* do until word is negative */
   while (off >= BLKSIZ) 
   {
      blk++;
      off -= BLKSIZ; 
   }
   zoff = off;
   zblk = blk;
}

getzchr(zchr, charset)
   ZIPINT zchr, charset;
{  /*   Given a 5 bit code in zchr and a character set number in charset,
   this routine returns the ASCII value of the char.
   */
   charset *= CSETLEN;              /* get offset into char set */
   zchr += charset - 6;             /* add in offset to char */
   return(character_set[zchr]);     /* look it up */
}

/************************************************************************
*                                                                       *
*   O U T P U T   B U F F E R I N G                                     *
*                                                                       *
************************************************************************/

void putchr(letter)
   ZIPINT letter;
{  /*   Putchr takes an ASCII character and queues it for output.  If it
   fills the buffer, a search backwards for a space is conducted to
   break the line.  The end of the buffer is determined by endbuf
   which should be set whenever the screen size is changed to reflect
   the difference between the right and left margins.

        if BUFOUT 0 is active (bufflg=0) then to hell with waiting,
        give the imp what he wants, in otherwords don't wait for
        glory to come to you, go out and get it
   */

/* I think this is the correct place to handle various output
   buffering options.
   The only one I am trying to impliment is TABLE buffer output.
   I also think that this is the only place we need to trap for TABLE 
   buffering. - ASK */

   int tw;

   if ( Tblout )
   { /* doing table output */
      tw = GTVWRD( PTWIDTH );
      PTVWRD( PTWIDTH, tw+CWIDTH(letter) );
      PTVBYT( Tblptr, letter );
		Tblptr++;
      Tblchars++;
   }
   else
   { /* just send it off to the screen */
		if ( letter == TAB || letter == EOS )
		{ /* send out a couple of spaces, at least til wrap */
			put_to_screen( SPACE );
			if ( CharX != (Win_xpos+Win_lmarg) )
			{ /* Kludgy way of checking for wrap, but it works */
				put_to_screen( SPACE );
			}
		}
		else
      	put_to_screen( letter );
   }
}

/* if last_space is set, it points to the buffer location of the most
   recently buffered space.  last_space_out, if 1, means that that space
   has been printed already, so probably not too good to back up to.
   However, if it's -1, then the space was the last thing dumped, so
   we can break by doing a carriage return, without dumping the buffer. */

#ifdef GERMAN
char german_letters[9] = {132, 148, 129, 142, 153, 154, 225, 175, 174};
char *german_strs[9] = {"ae", "oe", "ue", "Ae", "Oe", "Ue", "ss", ">>", "<<"};

cvt_putc(c)
   char c;
{
   char *seq;
    
   if ((c >= LOW_GERMAN) && (c <= HIGH_GERMAN)) 
   {
      if (!language_ok) 
      {
         seq = german_strs[c - LOW_GERMAN];
         while (*seq) 
         {
            md_putc(*seq++); 
         }
         return;
      }
      c = german_letters[c - LOW_GERMAN];
   }   

   md_putc(c);
}   

put_german(letter)
   ZIPINT letter;
{
   char *seq;

   seq = german_strs[letter - LOW_GERMAN];
   while (*seq) 
   {
      put_to_screen(*seq++); 
   }
}

#endif

void cr_int()
{
   /* do the CRCOUNT, CRFUNC thing */
   int count = wattrib[Screen].crintctr;

   if (count != 0)
   { /* must be some kind of counter */
      count--;
      wattrib[Screen].crintctr = count;
      if ( count == 0 )
      { /* time to make the internal call */
         internal_call(wattrib[Screen].crintfcn);
      }
   }   
}

void newlin()
{  /*    Newlin is called when a CRLF is desired on output of a zstring.
   It flushes the buffer and resets the buffer character pointer.
   */

   if ( Tblout )
   { /* doing table output, so just insert CR into table and quit */
      putchr(CR);
   }
   else
   { /* slap a null, and then dump the buffer */
      *Chrptr++ = NULL;      /* indicate end of line */
      dumpbuf();
   }
}

/* in EZIP there is no print buffering, and scripting is handled
   at the physical character output level */

void dumpbuf()
{  /*   Dumpbuf flushes the existing buffer to the screen. */

   if ( Chrptr != Outbuf ) 
   { /* just make sure one last time there is something to print */
   	Chrptr = Outbuf;      /* reset buffer pointer */
      if ( FTblout )
      { /* going to a formatted table, so fix it up */
         ftblprnt();
      }
      else
      { /* just print it then */
         mprnt( Outbuf );      /* this will ignore screen if off */
      }
   }
}


void print_number(num,prtfunc)
   int num;
   void (*prtfunc)( int );
{
   int modulator = 10000;

   if (num == 0)  
   {
      (*prtfunc)('0');
      return; 
   }
   if (num < 0)  
   {
      (*prtfunc)('-');
      num = -num; 
   }
   while (modulator > num)
      modulator /= 10;

   while (ZTRUE) 
   {
      (*prtfunc)('0' + (num / modulator));
      num %= modulator;
      modulator /= 10;
      if (modulator == 0) 
         break;
   }
}

void errprnt(buf)
char *buf;
{
   int savfont = Scrfont;
	int savtout = Tblout;
	int savftout = FTblout;
	int savvidflg = Vidflg;
	int savscr = Screen;
	int savscripting = Scripting;

	newdoscreen( 0 );
	Tblout = FTblout = Scripting = ZFALSE;
	Vidflg = ZTRUE;
   Scrfont = 1;         /* scrfont = 1 */

   while (*buf) 
   {
#ifdef GERMAN
      cvt_putc(*buf);
#else
		put_to_screen( *buf );
#endif
      buf++; 
   }
	put_to_screen( Z_EOL );
	dumpbuf();

	newdoscreen( savscr );
   Scrfont = savfont;
	Tblout = savtout;
	FTblout = savftout;
	Vidflg = savvidflg;
	Scripting = savscripting;
}

void gamprnt(buf)
   char *buf;
{ /* print a null-terminated string.  No formatting characters allowed! */
   errprnt(buf);
}

void script_string(buf)
   char *buf;
{
   if (Scripting && (Win_attributes & SCRIPTMASK))  
   {
      while ((*buf) && (*buf != Z_EOL)) 
      {
         md_script_char(*buf);
         buf++;
      }   
   }
}

void script_input(ibuf)
   ZIPINT ibuf;
{
   char chr;
   int count, i;
    
   if (Scripting && (Win_attributes & SCRIPTMASK))  
   {
      count = GTVBYT(ibuf + 1) + 2;
      for (i = 2; i <= count; i++) 
      { 
         if (((chr = GTVBYT(ibuf + i)) == NULL) || (chr == Z_EOL))
            break;
         md_script_char(chr);
      }
   }   
}

void mprnt( buf )
   char *buf;
{  /*   Mprnt prints a string assuming that Z_EOL indicates end of line
   without crlf and null requests a crlf.
   */
   char *sbuf;

   for ( sbuf = buf; *buf && (*buf != Z_EOL); buf++ )
   { /* print til end of line */
      if ( Vidflg ) 
      { /* there is some video, so show character */
         md_putc(*buf);      
      }   
   }

   script_string(sbuf);
   if (*buf == NULL)  
   { /* do <CR> then */
      mcrlf();             /* windowed scroll on ending null */
      cr_int();            /* check for <CR> function */
   }
}

/* zmprnt is identical to zprnt, but takes a ZIPCHAR *ptr */

void zmprnt(buf)
   ZIPCHAR *buf;
{  /*   Mprnt prints a string assuming that Z_EOL indicates end of line
   without crlf and null requests a crlf.
   (EZIP.  May have to be modified to support the printing of
   attributes.)
   */
   while ((*buf) && (*buf != Z_EOL))    /* search for line terminator */
      md_putc(*buf++);         /* print character */

   if (*buf == NULL) 
      mcrlf();            /* windowed scroll on ending null */
}

void chkscript()
{  /*    Chkscript is called when the flag scrchk has been set.
   Scrchk flag is set in BOR and BAND to indicate that a possible change
   in state of Scripting has occurred.  The flag should be set by
   OPDIROUT in EZIP.
   */   
   ZIPINT temp;
   char filename[PATHSIZ];

   Scrchk = 0;            /* reset flag */
   temp = GTVWRD(PFLAGS);      /* get status word */

   if ( Scripting || Scrhld )  
   { /* set according to current state */
      if ((temp & SCRIPTBIT) == 0)  
      { /* turn it off */
			script_off();
      }   
   }
   else if (temp & SCRIPTBIT)  
   { /* try and turn it on */
      gamprnt("Script to (Default is printer, or enter file name): ");
		mcrlf();
      if (md_getl(filename, PATHSIZ, 0, 0, 0) == 0)  
      { /* use the printer */
         mcrlf();
         Printabt = ZFALSE;
         if ( Dosprint )
			{ /* use dos printing */
				File_script = ZFALSE;
				Scrptfd = 0;
			}
         else 
         { /* negative script fd means printer */
            if (( Scrptfd = md_find_printer()) != 0)
				{ /* okay, we found one, not floppy scripting */
					File_script = ZFALSE;
				}
            else 
            { /* turn scripting off */
               script_off(); 
            }
         }
      }   
      else 
      { /* try and open the script file */
         mcrlf();
         Floppy_script = ZFALSE;
         md_check_swap(filename);
         if (Swapped && Swap_to_floppy && (!No_paging)) 
         {
            gamprnt("Can't script to another floppy on one-floppy system.");
            mcrlf();
            script_off();
            Swapped = ZFALSE;
            if ( Gamechn < 0 ) 
               md_get_game_chan( 0 );
            PTVWRD(PFLAGS, GTVWRD(PFLAGS) ^ FSTAT);
            return; 
         }
         if (Swapped)
            Floppy_script = get_fname_drive(filename) + 1;

         Swapped = ZFALSE;
         if ( Gamechn < 0 )
            md_get_game_chan( ZFALSE );

			Printabt = Diskabt = ZFALSE;
         if ((Scrptfd = zopen(filename, 1)) >= 0) 
         { /* foudn it, so append then */
            do_seek(Scrptfd, 2, 0);
            gamprnt("Appending to existing file.");
            mcrlf();
				File_script = ZTRUE;
         }   
         else if ( Printabt || Diskabt || (Scrptfd = zcreat(filename)) < 0) 
         { /* couldn't open it */
            script_off();
            gamprnt("Couldn't open script file.");
            mcrlf();
         }
			else
			{ /* show scripting to a file */
				File_script = ZTRUE;
			}
      }
   }
	if ( Scrptfd >= 0 )
	{ /* must have turned it out, so show world */
		Scripting = ZTRUE;
	}
}

void flush_buffer()
{
   if ( Chrptr != Outbuf )  
   { /* there's actually something there to flush */
      put_to_screen(Z_EOL);
      dumpbuf();
   }
}

not_terminator(c)
   unsigned char c;
{  
   ZIPINT trmtab;
   unsigned char ctchar;

   trmtab = GTVWRD(PTCHARS);
   if (trmtab != 0)  
   {
      bspltb(trmtab);
      while ((ctchar = getbyt()) != 0) 
      {
         if (c == ctchar) 
            return(ZFALSE);
         if ((c >= 128) && (ctchar == 255)) 
            return(ZFALSE);
         /* handle 255, meaning all function keys terminate */
      }   
   }
   return(ZTRUE);
}

void script_off ()
{
   Scripting = ZFALSE;
   if ( File_script )
      zclose( Scrptfd );

   Scrptfd = -1;
	Floppy_script = ZFALSE;

   PTVWRD(PFLAGS, GTVWRD(PFLAGS) & (~SCRIPTBIT));
}

/* just move Outbuf into table, filling out with necessary # of spaces */
void ftblprnt()
{
   int tchar = 0;
   int twid = 0;
   char *cp;
   ZIPINT tptr = Tblptr;

   Tblptr += 2;               /* save room for count (which is a word) */

   for ( cp = Outbuf; *cp && *cp != Z_EOL; cp++ )
   { /* so move it out little doggies */
      PTVBYT( Tblptr++, *cp );
      twid += CWIDTH( *cp );
      tchar++;
   }
   for ( twid += CWIDTH( SPACE ); twid < FTblwidth; twid += CWIDTH( SPACE ) )
   { /* go put some spaces out there */
      PTVBYT( Tblptr++, SPACE );
      tchar++;
   }
	
   /* now show how many bytes used in the table line */
   PTVWRD( tptr, tchar );

   /* reset CharX */
   CharX = 0;
}                                                 
       

void put_to_screen(letter)
   ZIPINT letter;
{
   int tempct;
   int linewid;

#ifdef GERMAN
   if ((letter >= LOW_GERMAN) && (letter <= HIGH_GERMAN)) 
   {
      if (language_ok) 
      {
         letter = german_letters[letter - LOW_GERMAN]; 
      }
      else 
      {
         put_german(letter);
         return;
      }   
   }
#endif

   linewid = CharX + CWIDTH(letter);

   if ( !(Win_attributes & WRAPMASK) && !FTblout )
   { /* not wrapping, but rather we are clipping */
      if ( linewid > Win_right )
      { /* we've done gone too far, so clip here and don't add char */
         *Chrptr++ = Z_EOL;
         dumpbuf();
			return;
      }   
   }   
   else
   { /* check for wrapping stuff */
      if ((!FTblout && (linewid > Win_right)) ||
          (FTblout && (linewid > FTblwidth)) )
      { /* must need wrap, so do it */
         if ( letter == SPACE )
         { /* space has caused wrap, so just put it in there */
            *Chrptr++ = SPACE;
            letter = PADCHR;        /* just put junk in there then */
         }

			if ( Chrptr == Outbuf )
			{ /* trying to wrap a non-existent line */
				*Chrptr++ = NULL;
				tempct = 0;
			}
			else
			{ /* check for space */
         	for ( tailptr = Chrptr-1; *tailptr != SPACE && tailptr != Outbuf;
            	tailptr-- )
            	/* look backwards for a space in the buffer */
            	;

         	if ( *tailptr != SPACE )
         	{ /* no live spaces anywhere, so just point to end */
            	tailptr = Chrptr;
         	}   

         	tempct = Chrptr - tailptr - 1;   /* get count ... */
         	*tailptr = NULL;
			}

         /* send buffer out into cruel world, with CR/LF */
         dumpbuf();                                        

			Chrptr = Outbuf;			/* reset Chrptr */
         while ( --tempct >= 0 ) 
         { /* from space+1 to the beginning of the output buffer */
            *Chrptr++ = *++tailptr;
            CharX += CWIDTH(*tailptr);
         }
      }   
   }

  	*Chrptr++ = (char)letter;
	*Chrptr = Z_EOL;
	if ( Vidflg || FTblout )
	{ /* only muck with CharX if actually displaying or doing formatted
		  table output */
   	CharX += CWIDTH( letter );
	}

   if ( !(Win_attributes & BUFFERMASK) && (!FTblout) )
   { /* no buffering, so just shove char out there */
      *Chrptr = Z_EOL;
      dumpbuf();
   }
}

void prt_tblf(tbl)
   ZIPINT tbl;
{
   char val;
   int num, num_chars;
    
	for ( num_chars = GTVWRD( tbl ); num_chars > 0; num_chars = GTVWRD( tbl ))
	{ /* do each line, please */
		tbl += 2;		/* skip the count */
      for (num = 0; num < num_chars; num++)
      { /* do each char */
         val = GTVBYT(tbl++);
         put_to_screen( val );
      }
		dumpbuf();
		locate((Scry+Font_height),(Win_xpos + Win_lmarg));
   }   
}

