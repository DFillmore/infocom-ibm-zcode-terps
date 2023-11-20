/************************************************************************
*									*
*	D I S K   I / O							*
*									*
************************************************************************/
#include "zipdefs.h"
#include "extern.h"
#include <io.h>
#include <dos.h>
#include <stdio.h>

getpre(first, final)	/* preload first through (final-1) */
short first, final;
{  /* 	Getpre is used by ZIPINI and RESTART to read in blocks.  The blocks
	are read to an offset in dataspace based on the virtual page number,
	as opposed to an "available" paging slot.
   */
    short i = first;
    ZIPCHAR *ptr = dataspace;
    short req,amt;
    short nblks = 63;			/* read up to 63 * 512 at once */
    lseek(gamechn, (long)(first*BLKSIZ), SEEK_SET);	/* access */
    while (ZTRUE) {
      if (i >= final) THEN break;
      if ((req = final - i) > nblks) THEN
	req = nblks;			/* # blocks to read */
      i += req;
      req *= BLKSIZ;			/* number of words */
      if ((i - 1) == game_blocks) THEN
	req -= (BLKSIZ - last_block_size);
      amt = far_read(gamechn, FP_SEG(ptr), FP_OFF(ptr), req);
      if (diskabt || (amt != req)) THEN
        fatal("Preload read failed");
      ptr += req;
      }
    return;
}

getblk(block, loc)
short block;
ZIPCHAR *loc;
{  /*	Getblk reads in a virtual block into absolute location loc.
   */
    long offset;
    short amount = BLKSIZ;
    if (block == game_blocks) THEN
      amount = last_block_size;			/* last block may be short */
    offset = (long)block << CVTBLK;		/* calculate seek offset */
#if _DEBUG
    if (debug & VERBOSE) THEN 
      printf("\nGetting block %d(%xh) at offset %lxh\n",
	    block,block, offset);
#endif
    lseek(gamechn, offset, SEEK_SET);	/* first seek to block */
    if ((far_read(gamechn, FP_SEG(loc), FP_OFF(loc), amount) != amount) ||
	diskabt) THEN
      fatal("Get block failed");	/* die on failed read */
    return;
}

wrtbyts(loc, numbyts)
ZIPCHAR *loc;
unsigned int numbyts;
{  /*  	This routine is used by SAV_RES to write numbyts from loc to the
	save file.
   */
    if ((far_write(savechn, FP_SEG(loc), FP_OFF(loc), numbyts) != numbyts) ||
	diskabt) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
}

rdbyts(loc, numbyts)
ZIPCHAR *loc;
unsigned int numbyts;
{  /*  Rdbyts is used to read numbyts bytes into loc from the save file.
   */
    if ((far_read(savechn, FP_SEG(loc), FP_OFF(loc), numbyts) != numbyts) ||
	diskabt) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
}
