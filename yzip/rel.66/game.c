/************************************************************************
*                                                                       *
*   G A M E   C O M M A N D S                                           *
*                                                                       *
************************************************************************/
#include <dos.h>
#include <stdlib.h>
#include <ctype.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"

/* put save file prompts in one place */
static char *Entstr = "Enter save file name:";
static char *Defstr1 = "(Default is ";
static char *Defstr2 = "): ";

internal_sav_res(fcn)
   int fcn;
{  /* save/restore to memory, if possible.  Save function will never
      be called unless we think there's enough memory to do it.  The
      restore function will never be called unless a save has already
      happened.  The buffer is allocated during initialization. */
   ZIPCHAR *tbuf;
   ZIPINT scrsave[7];
   if (Isave_buffer)
   { /* we have the memory for this */
      save_screen_stuff(&scrsave[0]);
      push_save_stuff();
      if (fcn == OPSAVE) 
         do_copy(0, (unsigned)zstack, FP_SEG(Isave_buffer), FP_OFF(Isave_buffer),
            (LSTACK * 2));   /* save the stack */
      else
         do_copy(FP_SEG(Isave_buffer), FP_OFF(Isave_buffer), 0, (unsigned)zstack,
            (LSTACK * 2));   /* restore the stack */
      if ( pop_save_stuff(0) )
      { /* we got the stuff we saved back */
         if (fcn == OPSAVE)
         { /* save the world */
            tbuf = Isave_buffer + (LSTACK * 2);
            /* save impure space */
            do_copy(FP_SEG(Dataspace), FP_OFF(Dataspace), FP_SEG(tbuf),
               FP_OFF(tbuf), GTVWRD(PPURBT));
            Isave_happened = 1;
            restore_screen_stuff(&scrsave[0]);
            return(1);
         }   
         else
         { /* restore the world */
            tbuf = Isave_buffer + (LSTACK * 2);
            do_copy(FP_SEG(tbuf), FP_OFF(tbuf),
               FP_SEG(Dataspace), FP_OFF(Dataspace), GTVWRD(PPURBT));
            restore_screen_stuff(scrsave);
            return(2);
         }
      }   
      else
         fatal("IRESTORE failed");
   }
   return(0);
}

void save_screen_stuff(scrsave)
   ZIPINT *scrsave;
{
   scrsave[0] = GTVWRD(PVERS1);
   scrsave[1] = GTVWRD(PFLAGS);
   scrsave[2] = GTVWRD(PSCRWD);
   scrsave[3] = GTVWRD(PHWRD);
   scrsave[4] = GTVWRD(PVWRD);
   scrsave[5] = GTVWRD(PFWRD);
   scrsave[6] = GTVWRD(PCLRWRD);
}

void restore_screen_stuff(scrsave)
   ZIPINT *scrsave;
{
   PTVWRD(PCLRWRD,scrsave[6]);
   PTVWRD(PFWRD,scrsave[5]);
   PTVWRD(PVWRD,scrsave[4]);
   PTVWRD(PHWRD,scrsave[3]);
   PTVWRD(PSCRWD,scrsave[2]);
   PTVWRD(PFLAGS,scrsave[1]);
   PTVWRD(PVERS1,scrsave[0]);
}

void push_save_stuff()
{
   PUSHZ(zpc1);         /* save important stuff */
   PUSHZ(zpc2);
   PUSHZ(zlocs);
   PUSHZ(zargct);
   PUSHZ(zorkid);
   zstack[0] = zsp - zstack;   /* relative stack pointer */
}

pop_save_stuff( pc_change )
   int pc_change;   
{
   zsp = zstack + zstack[0];   /* absolute stack pointer */
   if (*zsp == zorkid)
   { /* we found things okay */
      POPZ();         /* zorkid */
      zargct = (char)POPZ();
      zlocs = POPZ();
      zpc2 = POPZ() & BYTEBITS;
      zpc1 = POPZ();
      if (!pc_change)
         newzpc();
      return(1);
   }   
   else
      return(0);
}

/* partial save and restore is called with
   fcn == OPSAVE or OPREST
   start points to a table in virtual memory
   length is the length in bytes
   name points to a table with length as the first byte...

   the doc specifies that we check that name is the same in the file being
   restored as the RESTORE's name argument. therfore this routine writes 
   the LTABLE as the first part of the file.
   the idea is the user might choose a different file name than what
   we promt him with.
*/

partial_sav_res(fcn,start,length,name) 
   int fcn;
   int start;
   int length;
   int name;
{
   char *fptr, filename[PATHSIZ];
   char chr;
   int j = 0, errcode, namlen, i, answer;
	int retval = ZFALSE;

   flush_buffer();
   md_hlite(0);
   gamprnt(Entstr);      /* print enter string */
   mcrlf();            /* windowed scroll */
   gamprnt( Defstr1 );   /* print last save file */

   /* make savfile = name  as default */
   bspltb(name);
   namlen = getbyt();
   for (i = 1; i <= namlen; i++)
   { /* fetch me the default name */   
      chr = getbyt();
      Savfile[j++] = chr;
   }   
   Savfile[j] = 0;
   md_display_default(Savfile, filename);
   gamprnt( Defstr2 );
	mcrlf();

   if (md_getl(filename, PATHSIZ, 0, 0, 0) == 0)  /* get a line of input */
      fptr = Savfile;            /* use default on crlf */
   else 
      fptr = filename;          /* otherwise use entered name */

   if (md_parse_file(fptr,filename,Savfile))
   {   
      mcrlf();
      gamprnt("Using ");
      gamprnt(filename);
      gamprnt(".");
   }

   if (Scripting)      /* script save file name if nec */
      script_string( filename );

   fptr = filename;
   mcrlf();
   md_check_swap(fptr);

   if (fcn == OPSAVE)          /* create or open accordingly */
	{ /* see if file already exists */
		if ( (Savechn = zopen( fptr, RDONLY )) >= 0 )
		{ /* see if user wants to delete file */
			zclose( Savechn );
			gamprnt("File already exists, delete it? ");
			answer = lc( md_inp() );
			if ( isprint( answer ) )
			{ /* show it */
				md_putc( answer );
			}
			md_mcrlf();

			if ( answer != 'y' )
			{ /* nope */
				Savechn = -1;
			}
			else
			{ /* show file 'not' found */
				Savechn = -2;
			}
		}

		if ( Savechn == -2 )
		{ /* -2 means file not found so just create it */
      	if ( (Savechn = zcreat(fptr)) < 0 )
			{ /* some kinda problem */
         	gamprnt("Couldn't create save file.");
         	mcrlf();
			}
		}
	}
   else
	{ /* check for restore */
      if ( (Savechn = zopen(fptr, RDONLY)) < 0 )
		{ /* some kind of problem with opening the file */
         gamprnt("Invalid save file");
         mcrlf();
		}
	}

	if ( Savechn >= 0 )
	{ /* doing just fine */
   	if (fcn == OPSAVE)
   	{ /* we are doing a save, so save name and length */
      	errcode=(far_write(Savechn,0,(unsigned)&namlen,sizeof(namlen))
         	!= sizeof(namlen));
      	if (!errcode && !Diskabt) 
         	errcode = (far_write(Savechn, 0, (unsigned)Savfile, namlen)
            	!= namlen);
   	}
   	else
   	{ /* doing restore, see if name matches */   
      	errcode = (far_read(Savechn, 0, (unsigned)&j, sizeof(j))
         	!= sizeof(j));
      	if (!errcode && !Diskabt)
      	{ /* we got size, go read in name */   
         	if (j > OBUFSIZ) 
            	j = OBUFSIZ;
         	errcode = (far_read(Savechn, 0, (unsigned)Outbuf, j) != j);
      	}
   	}

   	if (errcode || Diskabt)
   	{ /* oh dear, problems */
      	zclose(Savechn);
      	if (fcn == OPREST)
      	{ /* problems with restore */
         	gamprnt("Can't read file.");
				Diskabt = errcode = ZTRUE;
      	}
   	}   
   	else if (fcn == OPSAVE)
		{ /* do a save */
      	errcode = wrtbyts(Dataspace + start, length);
      	retval = ZTRUE;      /* return success */
		}
   	else
   	{ /* restoring, make sure the name matches */   
      	if ((j != namlen) || !same_name(Outbuf, Savfile, j))
      	{ /* nope, it doesn't match; save file incorrect */   
         	md_sound(FEEP);
         	gamprnt("Not an Infocom save file. RESTORE aborted.");
         	mcrlf();
      	}   
      	errcode = rdbyts(Dataspace + start, length);
      	retval = length;
   	}

   	zclose(Savechn);
   	if ((errcode == ZFALSE) || Diskabt)
   	{ /* problems, see what it might have been */   
      	if ( fcn == OPREST && errcode == ZFALSE )
			{ /* bad restore read, so croak */
         	fatal("Partial read on restore.");
			}
      	else
      	{   
         	gamprnt("Can't write file.");
         	if (!Diskabt)
            	gamprnt(" Disk full?");
         	do_delete(fptr);
      	}
			retval = ZFALSE;
   	}
	}
   md_check_unswap();
   md_install_default(fptr,Savfile);   /* save the name used... */
	return( retval );
}

same_name(buf, file, len)
int len;
char *file, *buf;
{ 
   int i;
   for (i = 0; i < len; i++)
   {   
     if (buf[i] != file[i])
      return(0);
   }  
   return(1);
}
   
sav_res(fcn)
int fcn;
{  /*   Save and restore routines.  This routine receives the OP as a
   parameter to distinguish whether it is reading or writing.  It
   first obtains a filename (after informing of the default).  It
   then pushes all vitals on the zstack (including the zsp) and
   r/w the zstack to disk.  Then it r/w's all the impure code to 
   disk.  It only informs the user of failure because the high-level
   indicate success with "Ok."

   The save file name is a global so that it may be retained between
   save/restores.

   (EZIP.  Modify to return values (see doc) rather than predicates)
  */
   char *fptr, filename[PATHSIZ];
   ZIPINT scrsave[7];
   int i, errcode, answer;
	int retval = ZFALSE;

   flush_buffer();
   md_hlite(0);
   gamprnt(Entstr);                 /* print enter string */
   mcrlf();                         /* windowed scroll */
   gamprnt( Defstr1 );              /* print last save file */
   md_display_default(Savfile, filename);
   gamprnt( Defstr2 );
	mcrlf();

   if (md_getl(filename, PATHSIZ, 0, 0, 0) == 0)  /* get a line of input */
      fptr = Savfile;               /* use default on crlf */
   else
      fptr = filename;              /* otherwise use entered name */

   if (md_parse_file(fptr, filename, Savfile))
   {   
      mcrlf();
      gamprnt("Using ");
      gamprnt(filename);
      gamprnt(".");
   }   
   else if (Scripting)       /* script save file name if nec */
      script_string(fptr);

   fptr = filename;
   mcrlf();
   md_check_swap(fptr);
   if (fcn == OPSAVE)       /* create or open accordingly */
	{ /* see if file already exists */
		if ( (Savechn = zopen( fptr, RDONLY )) >= 0 )
		{ /* see if user wants to delete file */
			zclose( Savechn );
			gamprnt("File already exists, delete it? ");
			answer = lc( md_inp() );
			if ( isprint( answer ) )
			{ /* show it */
				md_putc( answer );
			}
			md_mcrlf();

			if ( answer != 'y' )
			{ /* nope */
				Savechn = -1;
			}
			else
			{ /* show file 'not' found */
				Savechn = -2;
			}
		}

		if ( Savechn == -2 )
		{ /* -2 means file not found so just create it */
      	if ( (Savechn = zcreat(fptr)) < 0 )
			{ /* some kinda problem */
         	gamprnt("Couldn't create save file.");
         	mcrlf();
			}
		}
	}
   else
	{ /* check for restore */
      if ( (Savechn = zopen(fptr, RDONLY)) < 0 )
		{ /* some kind of problem with opening the file */
         gamprnt("Invalid save file");
         mcrlf();
		}
	}

   if ( Savechn >= 0 )
   { /* if sucessful, save stack */
      save_screen_stuff(&scrsave[0]);
      push_save_stuff();         /* save vitals */
      if (fcn == OPSAVE)      /* r/w stack */
         errcode = (far_write(Savechn, 0, (unsigned)zstack, LSTACK*2)
            == (LSTACK * 2));
      else
         errcode = (far_read(Savechn, 0, (unsigned)zstack, LSTACK*2)
            == (LSTACK * 2));

      if (!Diskabt && errcode )
      { /* I/O succeeded */
         if (pop_save_stuff (1))
         { /* will check zorkid */
            if (fcn == OPSAVE)
            { /* r/w impure code accordingly */
               errcode = wrtbyts(Dataspace, GTVWRD(PPURBT));
            }
            else
            {   
               errcode = rdbyts(Dataspace, GTVWRD(PPURBT));
            }
            restore_screen_stuff(&scrsave[0]);
            zclose(Savechn);         /* close the file */
            if ((errcode != ZFALSE) && !Diskabt)
            { /* fine, everything seemed to work */
               md_install_default(fptr,Savfile);
               if (fcn == OPREST) 
                  retval = 2;
               else
                  retval = ZTRUE;      /* return success */
            }   
            else if (fcn == OPREST)
            { /* restore failure is fatal */
               fatal("Partial read on restore");
            }
         }
         else
         { /* bad zork id */
            fatal("Wrong game or version");
         }
      }
      else
      { /* stack I/O didn't work, so just forget it all */
         for (i = 1; i <= 5; i++)
            POPZ();
      }
   	if ( (fcn == OPSAVE) && !retval )
   	{ /* problems with save, so complain */   
      	do_delete(fptr);
      	gamprnt("Couldn't write save.");
      	if ( !Diskabt )
         	gamprnt(" Disk full?");
      	mcrlf();
   	}
   }

	/* do final type stuff for save-restore */
   md_check_unswap();
   newzpc();
   return retval;
}
/* delete a file */
void do_delete(filename)
   char *filename;
{
   union REGS regs;
   regs.h.ah = 0x41;
   regs.x.dx = (unsigned)filename;
   int86(0x21, &regs, &regs);
}

verify()
{  /*   Verify computes a checksum on the entire data file, less the header.
   All pages are brought in from disk.  The checksum is then compared 
   to the checksum stored in the header. (EZIP - Remove annoucing printf)
   */
   ZIPBYT buffer[BLKSIZ];      /* buffer to use */
   ZIPINT chksum = 0, blocksum();
   short i, lastblk, lastoff;

   bsplito( GTVWRD(PLENTH) );   /* get length of game file */
   lastblk = zblk;
   lastoff = zoff;

   chksum += blocksum(buffer, 0, HDRSIZ, BLKSIZ); /* skip the header bytes */
   for ( i=1; i < lastblk; i++)
      chksum += blocksum(buffer, i, 0, BLKSIZ);

   chksum += blocksum(buffer, lastblk, 0, lastoff);/* sum the final bytes */

   if (chksum == GTVWRD(PCHKSM))       /* desired checksum */
      return(ZTRUE);
   else 
      return(ZFALSE);
}

ZIPINT
blocksum (buffer, block, off1, off2)   /* checksum a block */
	ZIPBYT buffer[BLKSIZ];
	short block, off1, off2;
{
   register ZIPINT sum = 0, i;

   lseek(Gamechn, (long)block << CVTBLK, 0);   /* seek to block */
   if ((far_read(Gamechn, 0, (unsigned)buffer, off2) != off2) || Diskabt)
      fatal( "Read block error");


   for ( i=off1; i < off2; i++)      /* sum between given offsets */
      sum += buffer[i];

   return(sum);
}

