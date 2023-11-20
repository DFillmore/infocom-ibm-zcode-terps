/* Beware!!!
In order to make this less bloated, as much as possible of the standard
C libraries has been cut.  In particular:
_setenvp, which parses the (unix-like) environment, has been replaced by
 a null routine.  So has _setargv, which parses the command line.  The
 init routine looks directly at the command line stored in the PSP.
 We get the PSP either by using the appropriate call (dos 3.0), or by
 looking very near the beginning of the code segment (dos 2.0, the cretins).
Also, __NMSG_WRITE, __NMSG_TEXT, __ctermsub, _exit, __exit, __dosret0,
 and __dosretax have been replaced by simpler (or null) versions, in
 LIB.ASM.  This allows the linker to not include the library modules
 dosret and crt0dat.
int86 and int86x have been replaced by simpler versions that don't do
 any fancy error stuff.
All i/o is done in FARREAD.ASM, via far_read, far_write, zopen, zcreat;
 they all leave something meaningful in ES, so the critical error
 handler (also defined there) can get a useful DS when it runs.  FARREAD
 also contains the low-level critical error handler, do_copy (for
 copying stuff around in FAR space), and get_segreg (which replaces
 segread).
*/

/* for TREE DIAGRAMMER
 <p>c:\bin\tc\include\
*/
/************************************************************************
*                                                                       *
*                                                                       *
*      TITLE:   Z-LANGUAGE INTERPRETIVE PROGRAM                         *
*                                                                       *
*           For the IBM-PC                                              *
*                                                                       *
*       Copyright Infocom (c)(p), 1985                                  *
*                                                                       *
*    Written/hacked by a whole host of characters, including:           *
*              Paul Gross                                               *
*              Ed Blackwell                                             *
*              Tim Anderson                                             *
*              Andy Kluzniacki                                          *
*              Phil Stanway                                             *
*              J. Scott Fray                                            *
*              Jonathan Arnold                                          *
*              John Fachini      (graphics.asm)                         *
*                                                                       *
************************************************************************/

#include <fcntl.h>
#include <stdlib.h>
#include <io.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <dos.h>
#include <ctype.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"
#include "sysdep.h"

static int memavail;   /* how much memory we think we have, in blocks */

/************************************************************************
*                                                                       *
*   M A I N   P R O G R A M   A N D   I N I T S                         *
*                                                                       *
************************************************************************/

void main( void )
{
   _fmode = O_BINARY;      /* set the default open mode to NOT TEXT */
   
   md_setup();             /* do some things once'd */

   /* init will return the display wanted by the user, if any */
   Display = init( Gamfile );        /* handle command line */

   md_init_screen();                /* re-set up screen */
   clear_screen();

   sysini();      /* do system initialization */
   memini();      /* setup memory, preload, paging, and tty */
   zipini();      /* setup zip header info and table pointers */

   /* this never returns */
   main_loop();
}

void _setargv()
{
   return;
}

void _setenvp()
{
   return;
}

void _nullcheck()
{
   return;
}

int init( name )
   char name[];
{
   ZIPCHAR *cmdlin;
   char datfile[15];
   char *ext = ".zip", *s;
   int i;
   int rdisplay = 0;
   int ccount;
   int locmem = 0;
   char c;
   unsigned char newname = ZFALSE;      /* ZTRUE if /g flag used */
   unsigned char noext = ZTRUE;         /* is there a passed .ext? */
   union REGS regs;

   regs.h.ah = 0x30;                  /* get DOS version */
   int86(0x21, &regs, &regs);
   if (regs.h.al < 3)
   { /* DOS < 3, gotta figger command line offset out myself */
      cmdlin = MK_FP( (get_segreg(1)-0x10), 0x80 );
   }
   else 
   { /* DOS >= 3, so ask it where command line is */
      regs.h.ah = 0x62;      
      int86(0x21, &regs, &regs);
      cmdlin = MK_FP( regs.x.bx, 0x80 );
   }
   /* get how many chars on command line*/
   if (*cmdlin & 0x80)
      ccount = -(*cmdlin);
   else
      ccount = *cmdlin;

   while ( ccount-- > 0 ) 
   { /* check the command line */
      c = *++cmdlin;
      if ( c == '/' || c == '-' )
      { /* passing some kind of parameter */
         c = *++cmdlin;
         ccount--;
         switch ( lc(c) )
         {
			case 'c':					/* font number */
				FontNo = (*++cmdlin) & 0x07;
				ccount--;
				break;
         case 'p':               /* script using dos instead of bios */
            Dosprint = ZTRUE;     
            break;
#ifdef MAN_MOUSE
         case 'm':               /* turn rodent on */
            Mouse_on = ZTRUE;
            break;
#endif
         case 'k':               /* how much memory allowed */
            locmem = 0;
            while (ccount-- > 0) 
            { /* read til end of line (maybe) */
               c = *++cmdlin;
               if ((c >= '0') && (c <= '9')) 
               { /* digit, so count it */
                  locmem *= 10;
                  locmem += c - '0'; 
               }
               else 
               { /* not a digit, so back up and continue */
                   ccount++;
                   cmdlin--;
                   break; 
               } 
            }
            locmem -= 40;      /* allow for zip storage */
            break; 
         case 'f':                     /* disallow UNDO */
            Isave_allowed = ZFALSE;
            break;
#if !defined( RELEASE )
         case 'g':                     /* specify gamefile */
            s = datfile;
            while (ccount-- > 0)
            { /* read to end of command line, maybe */
               c = *++cmdlin;
               if ( c == 0 || isspace(c) )
               { /* some kind of white space, so quit */
                   break; 
               }
               else if ( c == '/' )
               { /* starting next parm, so back up */
                  ccount++;
                  cmdlin--;
                  break;
               }
               newname = ZTRUE;         /* just set this if we have name */
               *s++ = c;
            }
            *s = 0;
            break;
#endif
         case 'd':                        /* what display? */
            ccount--;
            switch ( lc(*++cmdlin) ){

            case 'm':                     /* MCGA */
               rdisplay = VID_MCGA;
               break;
            case 'e':                     /* EGA */
               rdisplay = VID_EGA;
               break;
            case 'c':                     /* CGA */
               rdisplay = VID_CGA;
               break;
            default:                      /* neither, so go back one char */
               ccount++;
               cmdlin--;
               break;
            }
            break;
#if !defined( RELEASE )
			case 'b':								/* picture buffer size */
				Pic_buff_size = 0;
            while (ccount-- > 0) 
            { /* read til end of line (maybe) */
               c = *++cmdlin;
               if ( isdigit( c ) )
               { /* digit, so count it */
                  Pic_buff_size *= 10;
                  Pic_buff_size += c - '0'; 
               }
               else 
               { /* not a digit, so back up and continue */
                   ccount++;
                   cmdlin--;
                   break; 
               } 
            }
				break;
#endif
         default: 
            mcrlf();
            gamprnt("Unknown switch: ");
            md_putc(c);
            mcrlf();
         /* fall thru to help screen and then die */
         case '?':               /* do help */
         case 'h':
            gamprnt("Usage: yzip [flags]");
            mcrlf();
            gamprnt("Where [flags] can be:");
            mcrlf();
            gamprnt(" /p = Script using DOS instead of BIOS");
            mcrlf();
#ifdef MAN_MOUSE
            gamprnt(" /m = Use mouse, if available");
            mcrlf();
#endif
            gamprnt(" /kn = Use [n]Kb of memory");
            mcrlf();
            gamprnt(" /f = Don't allow UNDO command");
            mcrlf();
            gamprnt(" /d[c,e,m] = Use display mode - c=CGA, e=EGA, m=MCGA");
            mcrlf();
            gamprnt(" /?, /h = This help display");
            mcrlf();
#if !defined( RELEASE )
            gamprnt(" /gfilename = Use gamefile 'filename'");
            mcrlf();
            gamprnt(" /bn = User [n]Kb for picture buffer");
            mcrlf();
#endif
				md_inp();
            z_exit( 1 );
            break;
         }
      }
   }

   if (locmem)
      Memreq = -(locmem * KTOBLKS);

	if ( Pic_buff_size )
	{ /* check size */
		if ( Pic_buff_size > MAX_PB_SIZE )
		{ /* too big */
			Pic_buff_size = MAX_PB_SIZE;
		}
		Pic_buff_size *= KTOBLKS;
	}

   if ( newname )
   { /* we have new name, so copy it in good spot */
      s = datfile;
   }
   else
   { /* used installed one */
      s = GameName;
   }

   for(i = 0; *s; s++,i++ )
   { /* so copy new name over */
      if (*s == '.')
         noext = ZFALSE;
      name[i] = *s; 
   }

   if ( noext )  
   { /* no extension was passed, so add default one on */
      s = ext;
      while (*s)
         name[i++] = *s++; 
   }
   name[i] = '\0';        /* NULL terminate string */

   return( rdisplay );
}


void sysini()
{   /*  Sysini opens the data file, saves away the name as the default save
   name, and determines total available memory.
   */
   int geebees, mq;
   ZIPCHAR *halloc();
   char *d, *s, *p, *ext = ".sav", *pic_ext;
   long filen();

   s = Gamfile;
   d = Savfile;
   p = Picfile;
   while (*s != PERIOD)
   { /* copy game file name */
      *d++ = *s;         
      *p++ = *s++;
   }

	if ( Display & VID_MCGA )
		pic_ext = MCGA_EXT;
	else if ( Display & VID_EGA )
		pic_ext = EGA_EXT;
	else
		pic_ext = CGA_EXT;

   do
   { /* copy two default extensions over */
      *p++ = *pic_ext++;
      *d = *ext++;
   } while ( *d++ );

	/* set a few things up for file opening */
   Swapdrive = get_fname_drive( Gamfile );
   if ( get_disk_type(Swapdrive) != -1 )
      Swap_to_floppy = ZTRUE;

	/* first, let's open picture file #1 */
   open_picture_file( Picfile, 1 );

	/* now, force getting game channel */
  	md_get_game_chan( ZTRUE );

   Datend = filen(Gamechn);      /* mem req size = file length */
   mq = (short)((Datend + BLKSIZ - 1) >> CVTBLK); /* round up */
   if ( Memreq == 0 )
	{ /* either no memory requested */
		Memreq = mq;
	}

   geebees = 700;   /* temporary scheme to account for game_blocks being 
               calculated in memini, but needing to allocate the
               memory before determining memavail ala md_msize() */

   pagedesc = (blkdesc far *)halloc( (sizeof(struct blkdesc) * geebees) /511);
   /* number of pages plus one */
   /* jsf928 */

   pagemap = (int far *)halloc((sizeof(int) * geebees)/511 + 1);/* allocate space for pagemap */
   /* jsf928 */

	/* allocate needed memory for pictures */
	if ( Pic_buff_size == 0 )
		Pic_buff_size = DEF_PIC_BUFF_SIZE;

   Pic_buff = (void far *)halloc( Pic_buff_size );
   Pic_buff_size *= (BLKSIZ / 16);				/* get me size in paragraphs */
   Hash_buff = (void far *)halloc( HASH_TABLE_SIZE );

   /* 640 pages is the max we'll need--assume a 256K game (512 pages) +
      64K undo area.  The extra 32 pages are for the display memory in
      Tandy graphics mode.  This allows us to compute accurately how much
      is available. *//* Up 640 to 1028 10/2/88 */
   memavail = md_msize(1168);
}

void memini()
{ /*   This routine compares memreq with ENDLOD and PLENTH.  It
   determines how much dataspace to allocate, and does so.  It determines
   how much data to preload, and does so.  It also initializes paging.
  */
   ZIPINT maxlod;
   short i;
   ZIPCHAR *fptr;
   short usebuf = 0;
   ZIPCHAR *halloc();
   ZIPCHAR *ndataspace;

   /*  Read the first block into a temporary buffer.  We temporarily set
   dataspace to point to this buffer, so that getpre() and the GTV macros
   work.  */
   Dataspace = Inbuf;
   lseek(Gamechn, 0L, SEEK_SET);      /* access to block 0 */
   if ((far_read(Gamechn,FP_SEG(Dataspace),FP_OFF(Dataspace),MIN_HEADER)!=
      MIN_HEADER) || Diskabt) 
      fatal("Unable to read file header");

   endlod = GTVWRD(PENDLD);      /* get endlod pointer */
   endlod = (endlod + BLKSIZ-1) >> CVTBLK; /* round & convert to blocks */

   maxlod = GTVWRD(PLENTH);      /* length of program, in words */
   bsplito( maxlod );            /* length word is now divided by 8, */
                                    /*   not 4 1002jsf */

   maxlod = zblk;                     /* number of blocks */
   last_block_size = BLKSIZ;
   if (zoff > 0)  
   { /* coutn last odd block */
      maxlod++;                     /* maybe one more */
      last_block_size = zoff;       /* remember odd-sized block */
   }
   game_blocks = maxlod - 1;            /* number of last block */

   /* here compute the number of blocks needed for an ISAVE, if the game and
   the user both want it.  If the memory available is big enough to make it
   fit, then allow it. */

   Isave_size = 0;
   if ((Isave_allowed) && (GTVWRD(PFLAGS) & FUNDO))  
   { /* game and user want to try UNDO */
      Isave_size = GTVWRD(PPURBT) + (LSTACK * 2); /* core needed for isave */
      bspltb(Isave_size);      /* get blocks and words in last */
      Isave_size = zblk;
      if (zoff > 0) 
          Isave_size++;         /* blocks for isave */
   }
   else
      Isave_allowed = ZFALSE;      /* turn it off */

   Fontlen = (int)(Datend - ((long)GTVWRD(PLENTH))*4);
   Datend -= Fontlen;   /* now lseek pointer for font data */

   /* number of bytes at end for font stuff */
   memavail &= ~BIT16;         /* unsigned */

   if ( Display & VID_TANDY )
   { /* save some room for picture buffer */
      memavail -= 32;      
   }

   if (Memreq < 0)  
   { /* user told us what to use */
      Memreq = -Memreq;         	/* make it positive */
		memavail = Memreq;			/* so set maximum available */
   }

   if (Memreq > memavail) 
      Memreq = memavail;

   if ( Isave_allowed )
   { /* check to see if we have the memory for it */
      if ( (memavail - Isave_size) < (endlod + 10) )
      { /* not enough for both preload & isave memory, loose isave */
         Isave_size = 0;         /* punt it */
         Isave_allowed = ZFALSE;
			gamprnt( "Not enough memory for UNDO" );
			mcrlf();
			md_inp();
      }
      else if ( (Memreq + Isave_size) > memavail )
      { /* save room for the isave and a little kludge */
         Memreq = memavail - Isave_size - 2;
      }
   }

   /* This hack ensures that we'll preload at least the C64 size if possible,
   thereby reducing swapping for games that get their main loop into the
   C64 preload.  We'll have at least two swapping pages (probably plus the
   two font buffer pages) in addition...
   */
   if ((endlod < 87) && (Memreq >= 89)) 
      endlod = 87;

   /*  Note that our paging scheme normally requires a minimum of 2 pages in 
   the chain, one for the current code page and a second for roving pointers.
   In the freak case where only one page is not preloaded, however, the
   "chain" may contain only one page too.  When all pages are preloaded, 
   paging is never called and no chain at all is required.  Thus an array
   of MAXBLKS paging structures is the most ever needed.
   */
   if ( Memreq < endlod )
   {
      fatal("Insufficient memory for preload"); 
   }
   else if (Memreq >= maxlod)  
   { /* mucho memory, take advantage */
      No_paging = ZTRUE;
      endlod = maxlod;            /* hack endlod to force total preload */
      Memreq = maxlod;            /* reduce Memreq to max needed */
   }

   if ((ndataspace = halloc(Memreq)) == (ZIPCHAR *)NULL)  
   { /* couldn't allocate the memory we wanted */
      gamprnt("Unable to allocate ");
      print_number(Memreq, md_putc);
      gamprnt(" blocks");
		md_inp();
      fatal("Memory allocation error");
   }

   if (Isave_allowed) 
   { /* check for enough room for isave buffer */
      if ( (Isave_buffer = halloc(Isave_size)) == NULL)
		{ /* sorry, can't really get the memory */
			gamprnt("UNDO is not available.");
			mcrlf();
			md_inp();
         Isave_allowed = ZFALSE;
		}
   }

   clear_screen();

   copyright();
   mcrlf();
   gamprnt("Game loading.  Please wait...");
   mcrlf();
	mcrlf();
	print_number( maxlod, md_putc );
	gamprnt(" pages requested.");
	mcrlf();
   print_number( memavail, md_putc );
   gamprnt(" pages available.");
   mcrlf();
   print_number( Memreq, md_putc);
   gamprnt(" pages allocated.");
   mcrlf();
	if ( Isave_allowed )
	{ /* print message */
   	print_number( Isave_size,md_putc);
   	gamprnt(" pages allocated for UNDO.");
   	mcrlf();
	}

	md_wait( 1 );

   Dataspace = ndataspace;
   getpre(0, endlod);         /* read in preload data */

   /*  Currently, an array of blkdescs and a pagemap are declared statically
   [0..255].  Should allocate space dynamically for [endlod..Memreq-1] only 
   (number of physical buffers), and a pagemap array for [0..maxlod-1] only
   (number of actual pages).

   IDEA:  call getpre(endlod, Memreq) to "prime" the page buffers, and 
   mark each pagedesc and pagemap appropriately.
   */
   if ( endlod < maxlod )  
   { /* if total preload, just skip */
      for (i = 0; i <= game_blocks; i++)
         pagemap[i] = NOT_IN_CORE;   /* no paged pages in core, yet */
      for (i = endlod; i < Memreq; i++) 
      { /* init double-linked list */
          pagedesc[i].next = i+1;      /* setup pointer chain */
          pagedesc[i].prev = i-1;      /* jsf928 */
          pagedesc[i].loc = (Dataspace + (long)(i * 512L));
          pagedesc[i].vpage = NO_PAGE;
      }
      if ( !(Display & (VID_EGA|VID_CGA)) && (i <= game_blocks)) 
      { /* use font buffer for more memory */
         fptr = MK_FP( get_segreg(2), 0 );
         for (usebuf = 0; usebuf < 2 && i <= game_blocks; i++,usebuf++) 
         { /* so set up PageDesc to use it */
            pagedesc[i].next = i+1;
            pagedesc[i].prev = i-1;   /* jsf928 */
            FP_OFF(fptr) = (unsigned)(&Main_font[0] + (BLKSIZ * usebuf));
            pagedesc[i].loc = fptr;
            pagedesc[i].vpage = NO_PAGE;
         }
         if ( i <= game_blocks ) 
         { /* use alternate font pages then */
            fptr = MK_FP( get_segreg(2), 0 );

            /* fill in page descriptors for the two pages used for fonts if
            we're not going to use them. */
            for (usebuf = 0; usebuf < 2 && i <= game_blocks; i++,usebuf++) 
            { /* init pagedesc, please */
               pagedesc[i].next = i+1;
               pagedesc[i].prev = i-1;   /* jsf928 */
               FP_OFF(fptr) = (unsigned)(Alt_font + (BLKSIZ * usebuf));
               pagedesc[i].loc = fptr;
               pagedesc[i].vpage = NO_PAGE;
            }
         }
      }
      i--;                         /* point at last one, please */
      pagedesc[i].next = endlod;   /* make the list circular */
      pagedesc[endlod].prev = i;   /* excluding pre and extra */
      mru = i;                     /* init mru to last page */
   }

}

void centerprt(str, lin)
   char *str;
   int lin;
{
   int i = strlen(str);

   locate( lin*FONT_HEIGHT, (Max_screen_width-(i*Zip_font_width))>>1 );
   gamprnt(str);
}

void copyright()
{
   centerprt("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",1);

   centerprt("Copyright (c) zzzz Infocom Inc.",2);

   locate( (5*FONT_HEIGHT), 0 );
   gamprnt("Created using Turbo C, V2.0");
   mcrlf();
   gamprnt("Copyright (c) Borland 1987,1988");
   mcrlf(); 
}

void zipini()
{ /*   ZIPINI initializes the ZIL world's link to the interpreter.  Pointers
   to each of the tables referenced are setup.  Type of status line, and
   interpreter capabilities are setup (split screen).  All of this
   initialization requires loading only the first game block, which
   contains a 64 byte header of game file information.

   Break characters for opread are setup as well.  Since they are defined
    in the vocab table's header, this requires loading the entire preload.

   (EZIP -- set interpreter id and version)
  */
   int mouse();
   ZIPINT extab, flags;
   short i;
   char *dest, *src2;
   ZIPINT count, source;

   if (GTVBYT(PVERS1) != ZMVERS)    /* check z-machine */
      fatal("Wrong Z-Machine version");

   if (GTVBYT(PVERS2) & 1)    /* check for byte swapped file */
      fatal("Byte swapped game file");


   PTVWRD(PCLRWRD, ((Ibm_to_zip[Def_bgcolor] << 8) | Ibm_to_zip[Def_fgcolor]) );

   extab = GTVWRD(PEXTAB);
#ifdef MAN_MOUSE
   if ( Mouse_on && (extab != 0) && (GTVWRD(extab) > 1))
#else
   if ( extab != 0 && (GTVWRD(extab) > 1))
#endif
   { /* no mouse stuff if we can't store the information anywhere */
      Mouseflg = ZTRUE;         /* we have a mouse (maybe) */
#ifdef MAN_MOUSE
		if ( md_mouse( INIT_MOUSE )
#else
      if ( md_mouse(FIND_MOUSE) && md_mouse( INIT_MOUSE ) )
#endif
      { /* found and init'd a mouse worked */
         if ( Display & VID_MCGA )
         { /* do special mcga handling for mouse cursor */
            md_mouse(SET_MOUSE_GRAPHICS_BLOCK); 
         }
         else
         { /* just deal with it correctly */
            md_mouse(SET_MOUSE_TEXT_CURSOR,0,0xffff,0x7700);
         }
         md_mouse(SET_MOUSE_INPUT_MASK, mouse, MOUSE_MASK);
      }
      else
      { /* show no mouse */
         Mouseflg = ZFALSE;
      }
   }

	flags = GTVWRD(PFLAGS);

	/* first, turn on some flags */
	flags |= (FDISP|FUNDO|FMOUS|FCOLO);

	if ( !Isave_allowed )
	{ /* turn off flag */
		flags &= ~FUNDO;
	}

	if ( !Mouseflg )
	{ /* show no mouse */
		flags &= ~FMOUS;
	}

	if ( !(Mode_bits & MODE_COLOR) )
	{ /* show no color */
		flags &= ~FCOLO;
	}

   PTVWRD(PFLAGS, flags );

   zorkid = GTVWRD(PZRKID);      /* get zork id */
   voctab = GTVWRD(PVOCTB);      /* set up vocab pointer */
   objtab = GTVWRD(POBJTB);      /* and the object table ptr */
   glotab = GTVWRD(PGLOTB);      /* and the globals table */
   wrdtab = GTVWRD(PWRDTB);      /* and the fwords table */

   purbot = GTVWRD(PPURBT);      /* get the purebot pointer */
   if (purbot & BYTEBITS)  
      purbot += BLKSIZ;         /* round up to next block */

   purbot >>= CVTBLK;         /* convert to blocks */

   main_vocabulary = voctab;
   count = GTVBYT(voctab);      /* first byte is number to transfer */
   source = voctab + 1;      /* source is an offset type ptr */
   dest = rbrks;

   for (i = 1; i <= count; i++)    /* transfer si break chars */
      *dest++ = GTVBYT(source++);

   esibrks = dest;
   src2 = irbrks;         /* end list with initial breaks */
   do
   {
      *dest = *src2++;
   } while ( *dest++ );      /* transfer up to and inc a null */

   mtime();            /* set up random seeds */

   yzipvers();         /* set yzip version numbers */

   /* tell the game what this hunk-o-junk can do */
   PTVBYT(PVERS2, Mode_bits);

   restart(ZFALSE);         /* continue ... */

}

void restart(midgame)
   int midgame;            /* FALSE if called from zipini() */
{
   int zstart;

   /*   Restart (also called by ZIPINI) reloads preload code, saves any flags
   that would be wiped out by the reload, and jumps to the game entry
   point.  (EZIP add in appropriate low memory settings.)
   */
   ZIPINT oldflags;
   ZIPINT oldcolors;
   ZIPBYT modebits;
   ZIPINT chrsetp;
   int chrsetc = 0;

   md_get_game_chan(1);      /* make sure we have the game disk */
   oldcolors = GTVWRD(PCLRWRD);

   if (midgame)
   { /* reload preload, jim */
      wind_init();         /* re-init each window */
      oldflags = GTVWRD(PFLAGS);
      getpre(0, endlod);
      PTVWRD(PFLAGS, oldflags);      /* save flags (SCRIPT) across reload */

      yzipvers();							/* install yzip version */
      PTVBYT(PVERS2, Mode_bits);        /* remind game of capabilities */
   }

   Vidflg = ZTRUE;                  /* let's have video, please */

   modebits = GTVBYT(PVERS2);
   modebits |= SPLTBIT;

   PTVBYT(PVERS2, modebits);
   PTVWRD(PSCRWD, ((Max_screen_height/FONT_HEIGHT)<<8)|(Max_screen_width/Zip_font_width));
   PTVWRD(PHWRD, Max_screen_width );
   PTVWRD(PVWRD, Max_screen_height );
   PTVWRD(PFWRD, (Zip_font_height << 8) | Zip_font_width );

   FOFF8 = (unsigned short) GTVWRD(PFOFF) * 8L;
   SOFF8 = (unsigned short) GTVWRD(PSOFF) * 8L;

   PTVWRD(PCLRWRD, oldcolors);

   Scrbtm = 0;

   clear_screen();
   locate( 0,0 );
   zsp = zstack + LSTACK;      /* setup stack pointers */
   Chrptr = Outbuf;         /* reset output buffer pointers */
   chrsetp = GTVWRD(PCHRSET);      /* Funny character set? */
   if (chrsetp) 
   { /* there is a funny character set */
      while(chrsetc < 78) 
      { /* So copy it in */
          character_set[chrsetc] = GTVBYT(chrsetp + chrsetc);
          chrsetc++; 
      } 
   }
   zlocs = zsp - zstack;      /* make a locals pointer */
   zlocs--;             /* to next stack slot*/

   /* YZIP METHOD OF GETTING STARTING ADDRESS */
   zstart = GTVWRD(PSTART);
   argblk[1] = zstart;
   argblk[0] = 1;
   docall(ZFALSE);
}

/* find length of file by seeking to end */
long
filen(fd) int fd; 
{
/*   return( filelength( fd ) );*/
#ifndef OLDWAY
   long curptr, endfile, lseek();
   curptr = lseek(fd, 0L, SEEK_CUR);   /* find current pos */
   endfile = lseek(fd, 0L, SEEK_END);   /* go to end */
   lseek(fd, curptr, SEEK_SET);   /* back to where we were */
   return(endfile);
#endif
}

char lc(ch)
   char ch;
{
   if ((ch >= 'A') && (ch <= 'Z'))
      ch += 'a' - 'A';

   return( ch );
}


void xpush(value,stack)      /* don't forget to comment this stuff */
   int value;
   ZIPINT stack;
{
   int temp;

   if ( (temp = GTVWRD(stack)) == 0)      /* if no space in stack */
      PRED(ZFALSE);                       /* cannot do fcn. */
   else
   { /* so stuff it in there */
      PTVWRD( (stack+(temp*2)), value);  /* stacks are word oriented */
      PTVWRD( stack, (temp-1) );          /* adjust stack[0] */
      PRED(ZTRUE);                        /* return fcn. completed */
   }
}

void pop(stack)
   ZIPINT stack;
{
   int offst;               /* offset into stack (loc. of val to be popped) */
   int popval;               /* value to be popped */

   if (argblk[0])            /* if user stack */
   {
      offst = GTVWRD(stack);         /* get offset into stack */
      popval = GTVWRD(stack+(2*offst) + 2);   /* get value to be popped */
      PTVWRD(stack,(offst+1));      /* adjust stack[0] */
      putval(popval);            /* return the popped value */
   }
   else
      putval(POPZ());            /* pop game stack */
}


void fstack(val,stack)
   int val;
   ZIPINT stack;
{
   int temp;

   if (argblk[0] != 2)            /* if no stack is specified */
      zsp+=val;            /* adjust game stack by val */
   else
   {
      temp = GTVWRD(stack);         /* get # of slots avail. in stack */
      PTVWRD(stack,(temp+val));      /* adjust by val */
   }
}

