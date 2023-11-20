/************************************************************************
*                                                                       *
*  P I C T U R E   H A N D L I N G                                      *
*                                                                       *
************************************************************************/
#include <stdlib.h>
#include <dos.h>
#include <errno.h>
#include <stdio.h>
#include <io.h>
#include <string.h>

#include "zipdefs.h"
#include "struct.h"
#include "pic.h"
#include "extern.h"
#include "sysdep.h"

/* local directory, as found in file */
static struct PF_LOCAL_DIR PFloc_dir[MAX_LOCAL_DIR];
static struct LOCAL_DIR PFentry;              /* fixed up entry */
static unsigned int huge *PFgbl_dir;      /* global directory, if any */
static int PFgbl_cnt;							/* how many in global */
static int PFdrive = -1;						/* drive picture file is in */
static struct PF_HEADER PFheader;             /* header for this file */

/* need to have two types of pointers cuz there could be two different
 types of picture directories - one with palette offset or one without. */
static struct PF_LD_PAL *PFld_pal;     /* this one has the palette offset */
static struct PF_LD_XPAL *PFld_xpal;   /* this one don't */

static char *PFname;                 /* name of the file */
static struct LOCAL_DIR PFentry;     /* local directory entry for picture */

static unsigned char Has_palettes;     /* TRUE if pictures might have a palette */
                                       /* thus using the bigger structures */
void picinf( picture, data )
   int picture;
   ZIPINT data;
{
   if ( picture == 0 )
   { /* looking for number of pictures and version */
      PTVWRD( data, PFheader.ld_count );
      PTVWRD( (data+2), ((PFheader.version >> 8) | (PFheader.version << 8)) );
		PRED( ZTRUE );
   }
   else
   { /* go get interesting information */
      if ( !get_dir_entry( picture ) )
		{ /* couldn't find the entry */
			PRED( ZFALSE );
		}
		else
		{ /* set the height/width */
      	PTVWRD( data, PFentry.height );
      	PTVWRD( (data+2), PFentry.width );
			PRED( ZTRUE );
		}
   }
}

void disp_pic(pict, y, x)
   int pict, y, x;
{
	int mstate = ZFALSE;

   if ( y == 0 )
      y = Scry;
   else
      y += (wattrib[Screen].ypos - 1);

   if (x == 0)
      x = Scrx;
   else
      x += (wattrib[Screen].xpos - 1);

   if ( !get_dir_entry( pict ) )
	{ /* couldn't find the picture */
		return;
	}

   if ( Has_palettes && PFentry.palette )
   { /* there is a palette with this picture, so load it */
      load_palette();
   }

   lseek( PFhandle, PFentry.offset, SEEK_SET );

	if ( Mouseflg && Mouse_shows )
	{ /* make sure mouse is off */
		mstate = Mouse_shows;
		md_mouse( HIDE_MOUSE );
	}

   disp_pic_f( x, y, PFentry.width, PFentry.height,
      Win_xpos+Win_xsize-1,
      Win_ypos+Win_ysize-1,
      PFhandle,
      PFentry.tr_color );

	if ( Mouseflg && mstate )
	{ /* reshow the mouse */
		md_mouse( SHOW_MOUSE );
	}
}

void load_palette()
{
   unsigned char pal_colors;
   unsigned char palbuf[50];
   int i;

   lseek( PFhandle, PFentry.palette, SEEK_SET );
   far_read( PFhandle, 0, (unsigned)&pal_colors, sizeof( unsigned char ) );

   if ( pal_colors )
   { /* there is a palette, so read it in */

      /* each color is 3 bytes */
      far_read( PFhandle, 0, (unsigned)palbuf, pal_colors * 3);

      for (i = (pal_colors*3)-1; i >= 0; i--)
      { /* move colors to lower 6 bytes in each byte */
         palbuf[i] >>= 2;
      }
      /* start new palette at color #2 */
      set_dacreg( 2, pal_colors, palbuf);    
   }
}

void reopen_pic_file()
{
	if ( PFhandle < 0 )
	{ /* it's not open, so open it */
		PFhandle = open_picfile( Picfile, PFheader.id );
	}
}

int open_picfile( name, id )
	char *name;
	int id;
{
	char ppath[PATHSIZ], drive;
	int handle;
	int ask = ZFALSE;
   union REGS regs;

	strcpy( ppath, name );

   do 
   { /* until we get it write! */
      regs.h.ah = 0xd;
      int86(0x21, &regs, &regs);
      handle = zopen( ppath, RDONLY);
      if ( handle < 0 )
      { /* didn't work yet */
			if ( PFdrive < 0 )
			{ /* haven't figgered out yet where it oughta be, so ask
				for it in the other drive */
         	if ( Swapdrive == 0 )
				{ /* no explicit drive, so use drive gamefile started in */
            	drive = Cur_drive + 'A';
				}
         	else
            	drive = Swapdrive + 'A' - 1;

				/* look for picture file in other drive */
				ppath[0] = (drive == 'A') ? 'B' : 'A';
			}
			else
			{ /* ask for it in the drive it started in */
				ppath[0] = PFdrive + 'A';
			}

			if ( ask )
			{ /* ask the question please */
				mcrlf();
         	gamprnt("Please put your ");
				if ( Display & VID_CGA )
					gamprnt( "CGA" );
				else if ( Display & VID_EGA )
					gamprnt( "EGA" );
				else
					gamprnt( "MCGA/VGA" );

				gamprnt( " PICTURES diskette, " );
#if defined(SHOGUN)
				if ( Display & VID_CGA )
					gamprnt( " Shogun disk #1," );
				else
					gamprnt( " Shogun disk #3," );
#elif defined(JOURNEY)
				if ( Display & VID_CGA )
					gamprnt( " Journey disk #1," );
				else if ( id == 1 )
					gamprnt( " Journey disk #3," );
				else 
					gamprnt( " Journey disk #4," );
#elif defined(ARTHUR)
				if ( Display & VID_CGA )
					gamprnt( " Arthur disk #1," );
				else if ( Display & VID_EGA )
					gamprnt( " for Arthur," );
				else
					gamprnt( " Arthur disk #3," );
#elif defined(ZORK0)
				if ( Display & VID_CGA )
					gamprnt( " Zork Zero disk #1," );
				else
					gamprnt( " Zork Zero disk #3," );
#endif
				gamprnt( " in drive ");
				md_putc( ppath[0] );
         	md_putc('.');
         	mcrlf();
            gamprnt("Press any key when ready, 'Q' to quit game: ");
            if ( lc(md_inp()) == 'q' )
				{ /* then just die */
					z_exit( 1 );
				}
         	mcrlf();
			}
			ask = ZTRUE;
			ppath[1] = ':';
			strcpy( &ppath[2], name );

      }   
   } while ( handle < 0 );

	/* get which drive picture file was found on */
	if ( (PFdrive = get_fname_drive( ppath )) <= 0 )
	{ /* current (default) drive, so see which one that is */
   	regs.h.ah = 0x19;					/* Get Current Disk */
   	int86(0x21, &regs, &regs);
		PFdrive = regs.h.al;				/* 0 == A, etc. */
	}
		
	return handle;
}

void open_picture_file( pfname, fid )
   char *pfname;
   int fid;
{
   int bytes;
   int ofile;

	ofile = open_picfile( pfname, fid );

   /* read in the global directory */
   far_read( ofile, 0, (unsigned)&PFheader, HEADER_LEN);

   if ( PFheader.id != fid )
   { /* sorry, but this does agree with what I want! */
      errprnt("File has wrong ID.");
      md_mcrlf();
      z_exit(1); 
   }

   /* close old file (if any) */
   zclose( PFhandle );

   if ( PFgbl_dir )
      hfree( (char huge *)PFgbl_dir );

   PFname = pfname;
   PFhandle = ofile;

   /* read in local directory */
   bytes = PFheader.ld_count * PFheader.ld_size;
   far_read( ofile, 0, (unsigned)&PFloc_dir, bytes );

   if ( PFheader.gblptr )
   { /* go get the global directory */
   	lseek( PFhandle, PFheader.gblptr, SEEK_SET );
		far_read( PFhandle, FP_SEG( &PFgbl_cnt ), FP_OFF( &PFgbl_cnt ), sizeof( int ) );
      PFgbl_dir = (unsigned int huge *)my_halloc( ((PFgbl_cnt*sizeof(int))/16)+1 );
      far_read( PFhandle, FP_SEG(PFgbl_dir), FP_OFF(PFgbl_dir), (PFgbl_cnt * sizeof( int )) );
   }

   if ( (Has_palettes = !(PFheader.flags & HDF_NO_PALETTES)) == ZFALSE )
   { /* use no palette structure pointer */
      PFld_xpal = (struct PF_LD_XPAL *)&PFloc_dir[0];
   }
   else
   { /* use the with palette structure pointer */
      PFld_pal = (struct PF_LD_PAL *)&PFloc_dir[0];
   }
   
}

struct PF_LOCAL_DIR *find_pic_id( id )
   int id;
{
   register int top;
   register int bot;
   register int cur;
   int cid;

   bot = 0;
   top = PFheader.ld_count;

   while (top > bot) 
   { /* look for the desired picture id */
      cur = bot + ((top - bot) >> 1);
      if ( Has_palettes )
         cid = PFld_pal[cur].id;
      else
         cid = PFld_xpal[cur].id;

      if ( cid == id)
      { /* found it */
         if ( Has_palettes )
            return( (struct PF_LOCAL_DIR *)&(PFld_pal[cur]) );
         else
            return( (struct PF_LOCAL_DIR *)&(PFld_xpal[cur]) );
      }
      else if ( cid < id)
         bot = cur + 1;
      else
         top = cur; 
   }
   return( NULL ); 
}

/* Do a binary search on the local directory for the specified picture.
   If it isn't there, search the global directory, and get the picture
   from the appropriate file. */

int get_dir_entry( id )
   int id;
{
   struct PF_LOCAL_DIR *local_dir;
   int fid;
   void open_picture_file();

   if ( (local_dir = find_pic_id( id )) == NULL )
   { /* can't find the entry locally, check for global */
      if ( PFgbl_dir != NULL && (fid = search_global_dir( id )) != 0 )
      { /* it's in the global directory, so open the right file and go get it */
			PFname[strlen(PFname)-1] = fid + '0';
         open_picture_file( PFname, fid );
         return ( get_dir_entry( id ) );
      }
      else
      { /* unable to find it */
			return ZFALSE;
      }
   }
   PFentry.id = local_dir->id;
   PFentry.width = local_dir->width;
   PFentry.height = local_dir->height;
   PFentry.flags = local_dir->flags;
   PFentry.offset = ((unsigned long)local_dir->dataptr[0]<<16) |
      ((unsigned int)local_dir->dataptr[1] << 8) | local_dir->dataptr[2];
   if ( Has_palettes )
   { /* might have a palette, so go get pointer */
      PFentry.palette = ((unsigned long)local_dir->palptr[0]<<16) |
         ((unsigned int)local_dir->palptr[1] << 8) | local_dir->palptr[2];
   }
   PFentry.tr_color = (local_dir->flags & LDF_TRANSPARENT) ?
      (unsigned char)(((unsigned short)local_dir->flags) >> 12) : 0xFF;

	return ZTRUE;
}

int search_global_dir( id )
   int id;
{
   register int bot, top, cur;
   int cid;
   
   bot = 0;
   top = PFgbl_cnt;

   while (top > bot) 
   { /* look through global directory for */
      cur = bot + ((top - bot) >> 1);
      cid = *(PFgbl_dir+cur) & GBL_DIR_ID;
      if ( cid == id)
         return GBL_FILE_ID( (*(PFgbl_dir+cur)) );
      else if ( cid < id)
         bot = cur + 1;
      else
         top = cur; 
   }
   return 0;
}

void set_dacreg(strt_color,amt_clrs,ptr_to_clrbuf)
   int strt_color;
   int amt_clrs;
   char *ptr_to_clrbuf;
{
   union REGS regs;

   regs.h.al = 0x12;
   regs.h.ah = 0x10;
   regs.x.bx = strt_color;
   regs.x.cx = amt_clrs;
   regs.x.dx = (unsigned)ptr_to_clrbuf;      /* palbuf */

   int86x(0x10, &regs, &regs, ES_REG, get_segreg( DS_REG ) );
}

