/************************************************************************
*                                                                       *
*  D I S K   I / O                                                      *
*                                                                       *
*************************************************************************/
#include "zipdefs.h"
#include "struct.h"
#include "extern.h"
#include <io.h>
#include <dos.h>
#include <stdio.h>

/* preload first through (final-1) */
void getpre(first, final)   
   short first, final;
{  /*    Getpre is used by ZIPINI and RESTART to read in blocks.  The blocks
   are read to an offset in dataspace based on the virtual page number,
   as opposed to an "available" paging slot.
   */
   short i = first;
   ZIPCHAR *ptr = Dataspace;
   short req,amt;
   short nblks = 63;         /* read up to 63 * 512 at once */

   lseek(Gamechn, (long)(first*BLKSIZ), SEEK_SET);   /* access */
   while (ZTRUE)
   {   
      if (i >= final)
         break;
      if ((req = final - i) > nblks)
         req = nblks;         /* # blocks to read */
      i += req;
      req *= BLKSIZ;         /* number of words */
      if ((i - 1) == game_blocks)
         req -= (BLKSIZ - last_block_size);
      amt = far_read(Gamechn, FP_SEG(ptr), FP_OFF(ptr), req);
      if (Diskabt || (amt != req)) 
        fatal("Preload read failed");
      ptr += req;
   }
}

void getblk(block, loc)
   short block;
   ZIPCHAR *loc;
{  /*   Getblk reads in a virtual block into absolute location loc.
   */
   long offset;
   short amount = BLKSIZ;

   if (block == game_blocks) 
      amount = last_block_size;         /* last block may be short */
   offset = (long)block << CVTBLK;      /* calculate seek offset */
   lseek(Gamechn, offset, SEEK_SET);   /* first seek to block */
   if ((far_read(Gamechn, FP_SEG(loc), FP_OFF(loc), amount) != amount) ||
               Diskabt)
      fatal("Get block failed");   /* die on failed read */
}

wrtbyts(loc, numbyts)
   ZIPCHAR *loc;
   unsigned int numbyts;
{  /*     This routine is used by SAV_RES to write numbyts from loc to the
   save file.*/

   if ((far_write(Savechn, FP_SEG(loc), FP_OFF(loc), numbyts) != numbyts) ||
            Diskabt) 
      return(ZFALSE);
   else
      return(ZTRUE);
}

rdbyts(loc, numbyts)
   ZIPCHAR *loc;
   unsigned int numbyts;
{  /*  Rdbyts is used to read numbyts bytes into loc from the save file.
   */
      
   if ((far_read(Savechn, FP_SEG(loc), FP_OFF(loc), numbyts) != numbyts) ||
            Diskabt)
      return(ZFALSE);
   else
      return(ZTRUE);
}
