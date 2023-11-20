/************************************************************************
*									*
*	D I S K   I / O							*
*									*
************************************************************************/
#include "zipdefs.h"
#include "extern.h"

getpre(first, final)	/* preload first through (final-1) */
short first, final;
{  /* 	Getpre is used by ZIPINI and RESTART to read in blocks.  The blocks
	are read to an offset in dataspace based on the virtual page number,
	as opposed to an "available" paging slot.
   */
    short i;
    ZIPCHAR *ptr;
    
    for (i = first, ptr = dataspace; i < final; i++, ptr += BLKSIZ) {
      getblk(i, ptr);
      }
    return;
}

getsblk(start, len, loc)
unsigned start;
int len;
ZIPCHAR *loc;
{
  long offset;
  offset = (long)start << 2;
  lseek(gamechn, offset, 0);		/* seek to right place */
  if (hread(gamechn, loc, len) != len) THEN
    fatal("Disk read failed");
  return;
}

getblk(block, loc)
short block;
ZIPCHAR *loc;
{  /*	Getblk reads in a virtual block into absolute location loc.
   */
    long offset;

    offset = (long)block << CVTBLK;		/* calculate seek offset */
#if _DEBUG
    if (debug & VERBOSE) THEN 
      printf("\nGetting block %d(%xh) at offset %lxh\n",
	    block,block, offset);
#endif
    lseek(gamechn, offset, 0);		/* first seek to block */
#ifdef IBMPC
    if (hread(gamechn, loc, BLKSIZ) != BLKSIZ) THEN	/* do the read */
      fatal("Get block failed");	/* die on failed read */
#else
    if (read(gamechn, loc, BLKSIZ) != BLKSIZ) THEN	/* do the read */
      fatal("Get block failed");	/* die on failed read */
#endif	/* IBMPC */
    return;
}

wrtbyts(loc, numbyts)
ZIPCHAR *loc;
int numbyts;
{  /*  	This routine is used by SAV_RES to write numbyts from loc to the
	save file.
   */
#ifdef IBMPC
    if (hwrite(savechn, loc, numbyts) != numbyts) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
#else
    if (write(savechn, loc, numbyts) != numbyts) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
#endif	/* IBMPC */
}

rdbyts(loc, numbyts)
ZIPCHAR *loc;
int numbyts;
{  /*  Rdbyts is used to read numbyts bytes into loc from the save file.
   */
#ifdef IBMPC
    if (hread(savechn, loc, numbyts) != numbyts) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
#else
    if (read(savechn, loc, numbyts) != numbyts) THEN
      return(ZFALSE);
    else
      return(ZTRUE);
#endif	/* IBMPC */
}
