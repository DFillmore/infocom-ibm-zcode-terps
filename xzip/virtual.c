/************************************************************************
*									*
*	V I R T U A L   M E M O R Y   R O U T I N E S			*
*									*
************************************************************************/

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"


PTAWRD(ptr, value)		/* set a word in preload */
ZIPCHAR *ptr;
ZIPINT value;
{
    *ptr = value >> 8;
    *(ptr + 1) = value;
}


ZIPINT nxtwrd()
{
    register ZIPINT high;	/* must EXPLICITLY get high byte first */

    high = nxtbyt() << 8;
    return (high | (nxtbyt() & 255));
}

ZIPINT purwrd(blk,off)
ZIPINT blk,off;
{
  ZIPINT val;
  ZIPCHAR *page;
  ZIPCHAR *getpag();
  while (off >= BLKSIZ) {
    blk++;
    off -= BLKSIZ; }
  page = getpag(blk);
  val = (*(page + off)) << 8;
  if (++off == BLKSIZ) THEN {
    off = 0;
    page = getpag(++blk); }
  return(val | ((*(page+off)) & 255));
}

/* Get a word from data space.  Addr is the byte address, has NO
   side effects. */
ZIPINT datwrd(addr)
unsigned int addr;
{
   ZIPINT val;
   ZIPCHAR *page;
   ZIPCHAR *getpag();
   short blk = addr >> CVTBLK;
   short off = addr & BYTEBITS;
   page = getpag(blk);
   val = (*(page+off)) << 8;
   if (++off == BLKSIZ) THEN {
     off = 0;
     page = getpag(++blk); }
   return(val | ((*(page+off)) & 255));
}

ZIPBYT datbyt(addr)
unsigned int addr;
{
  short blk = addr >> CVTBLK;
  short off = addr & BYTEBITS;
  ZIPCHAR *page;
  ZIPCHAR *getpag();
  page = getpag(blk);
  return(*(page + off));
}

ZIPBYT getbyt()
{  /*  	This routine takes its arguments in the globals zblk and zoff.
	It returns the indicated byte and updates the globals.
   */
    register ZIPBYT value;
    ZIPCHAR *page;
    ZIPCHAR *getpag();

/*  Testing for preload here avoids unneeded calls to getpag */

    if (zblk < endlod) THEN
      page = dataspace + ((long)zblk << CVTBLK);
    else page = getpag(zblk);

    value = *(page+zoff);	/* get the byte */

    zoff++;			/* update byte and block pointers */
    if (zoff == BLKSIZ) THEN {
      zoff = 0;
      zblk++;
      }
    return(value);		/* and return the byte */
}

ZIPINT getwrd()
{
    register ZIPINT high;	/* must EXPLICITLY get high byte first */

    high = getbyt() << 8;
    return (high | (getbyt() & 255));
}

newzpc()
{  /* 	Newzpc is called whenever the zpc might have crossed a block boundary.
	Normalize the zpc and get new page (if needed).
   */

    zpc1 += (zpc2 >> CVTBLK);	/* normalize (works if negative too) */
    zpc2 &= BYTEBITS;

    if (zpc1 != curblk) THEN {	/* crossed into new page? */
      curblk = zpc1;		/* update the zpc globals */
      curblkloc = getpag(zpc1);
      }
}
    
ZIPCHAR *getpag(blk)	/* return a pointer to the requested page */
short blk;
{
   /*	This is the heart of the paging scheme.  It manages a doubly-linked
	list of block descriptors.  Preloaded pages are not included in this
	list so they cannot be paged out.  If the page requested is preloaded,
	a valid pointer is returned immediately.  

	Otherwise, the block is removed from the linked list, spliced into
	the front of the list and made mru.  There are two subroutines, unlink
	and relink, that manage the linked list.

	If the block is not in core, the current mru's->previous (or lru
	block) buffer is used to page in the requested block.  Then the
	information in the corresponding block descriptors is filled to
	indicate the absence of the lru and the presence of the new.  The mru
	pointer is then pointed at this block.
   */

    struct blkdesc *lru;
    short deadpage;

#if _DEBUG
    if (blk >= MAXBLKS) THEN		/* valid block request? */
      fatal("Virtual page number out of range");
#endif

    if (blk == curpag) THEN		/* same page as last time */
      return(curpagloc);		/* return immediately */

    if (blk < endlod) THEN {		/* preloaded, expand the pointer */
      curpag = blk;
      curpagloc = dataspace + ((long)blk << CVTBLK);
      return(curpagloc); 
      }

    if (pagemap[blk] == NOT_IN_CORE) THEN {

/* When choosing the (lru) page to discard, make sure it's not the 
   current code page (where the zpc is), otherwise the newzpc buffer pointer 
   becomes invalid! -- DBB */

      lru = mru->prev;			/* get oldest page */
      deadpage = lru->vpage;
      if (deadpage == curblk) THEN {	/* but avoid using the zpc page */
        lru = lru->prev;		/* get next oldest page */
	deadpage = lru->vpage;
	}
      getblk(blk, lru->loc);		/* read new page over lru page */
      if (deadpage != NO_PAGE) THEN	/* mark old page as gone */
        pagemap[deadpage] = NOT_IN_CORE;
      pagemap[blk] = lru;		/* update map for new page */
      lru->vpage = blk;			/* update desc for new page */
      mru = lru;			/* update mru */
      }

    else				/* page is resident */
      if (pagemap[blk] != mru) THEN {	/* if already mru, do nothing */
	unlinkb(blk);			/* unsplice from wherever it is */
	relinkb(blk);			/* link it in as new mru */
	}

    curpag = blk;			/* update page globals */
    curpagloc = mru->loc;
    return(curpagloc);			/* return pointer */
}

unlinkb(block)
short block;
{  /* 	Unlink removes a block descriptor from the lru chain.
   */
    struct blkdesc *t1, *t2;

    t1 = pagemap[block]->prev;		/* get pointer to one end */
    t2 = pagemap[block]->next;		/* and the other */
    t1->next = t2;			/* swap pointers */
    t2->prev = t1;
}

relinkb(block)
short block;
{  /*  Splice a block back into the lru chain (becomes the new mru).
   */
    struct blkdesc *newblk, *lru;

    newblk = pagemap[block];		/* pointer to the splice block */
    lru = mru->prev;			/* lru pointer */
    newblk->next = mru;			/* update new desc's prev and next */
    newblk->prev = lru;
    mru->prev = newblk;			/* update mru and lru descs */
    lru->next = newblk;
    mru = newblk;			/* new mru */
}

bspltb(bytaddr)
ZIPINT bytaddr;
{  /*	Bspltb takes a (virtual) byte pointer, separates it into byte and 
	block pointers and returns them in the zblk and zoff globals.
   */
    zblk = bytaddr >> CVTBLK;		/* extract block bits */
    zoff = bytaddr & BYTEBITS;		/* extract byte offset bits */
}

bsplit(wrdaddr)
ZIPINT wrdaddr;
{  /* 	Bsplit takes a word aligned pointer, breaks it into a byte and
	block pointer, and returns them in zblk and zoff.
   */
    zblk = wrdaddr >> 8;		/* isolate block bits */
    zoff = (wrdaddr << 1) & BYTEBITS;	/* convert word offset to byte */
}

bsplitq(quadaddr)
ZIPINT quadaddr;
{  /* 	Bsplitq takes a quad aligned pointer, breaks it into a byte and
	block pointer, and returns them in zblk and zoff.
   */
    zblk = quadaddr >> 7;		/* isolate block bits */
    zoff = (quadaddr << 2) & BYTEBITS;	/* convert word offset to byte */
}


