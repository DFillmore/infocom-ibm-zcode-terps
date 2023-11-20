/************************************************************************
*                                                                       *
*   V I R T U A L   M E M O R Y   R O U T I N E S                       *
*                                                                       *
************************************************************************/

#include <dos.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"


void PTAWRD(ptr, value)      /* set a word in preload */
   ZIPCHAR *ptr;
   ZIPINT value;
{
   *ptr = (char)(value >> 8);
   *(ptr + 1) = (char)value;
}


ZIPINT nxtwrd()
{
   register ZIPINT high;   /* must EXPLICITLY get high byte first */

   high = nxtbyt() << 8;
   return (high | (nxtbyt() & 255));
}

ZIPINT purwrd(blk,off)
   ZIPINT blk,off;
{
   ZIPINT val;
   ZIPCHAR *page;

   while (off >= BLKSIZ)
   {   
      blk++;
      off -= BLKSIZ;
   }   
   page = getpag(blk);
   val = (*(page + off)) << 8;
   if (++off == BLKSIZ)
   {   
      off = 0;
      page = getpag(++blk);
   }   
   return(val | ((*(page+off)) & 255));
}

/* Get a word from data space.  Addr is the byte address, has NO
   side effects. */
ZIPINT datwrd(addr)
   unsigned int addr;
{
   ZIPINT val;
   ZIPCHAR *page;

   short blk = addr >> CVTBLK;
   short off = addr & BYTEBITS;
   page = getpag(blk);
   val = (*(page+off)) << 8;
   if (++off == BLKSIZ)
   {   
      off = 0;
      page = getpag(++blk);
   }   
   return(val | ((*(page+off)) & 255));
}

ZIPBYT datbyt(addr)
   unsigned int addr;
{
   short blk = addr >> CVTBLK;
   short off = addr & BYTEBITS;
   ZIPCHAR *page;

   page = getpag(blk);
   return(*(page + off));
}

ZIPBYT getbyt()
{  /*     This routine takes its arguments in the globals zblk and zoff.
   It returns the indicated byte and updates the globals.
   */
   register ZIPBYT value;
   ZIPCHAR *page;
   ZIPCHAR *getpag();

/*  Testing for preload here avoids unneeded calls to getpag */

   if (zblk < endlod) 
      page = Dataspace + ((long)zblk << CVTBLK);
   else
      page = getpag(zblk);

   value = *(page+zoff);   /* get the byte */

   zoff++;         /* update byte and block pointers */
   if (zoff == BLKSIZ)
   {   
      zoff = 0;
      zblk++;
   }   
   return(value);      /* and return the byte */
}

ZIPINT getwrd()
{
   register ZIPINT high;   /* must EXPLICITLY get high byte first */

   high = getbyt() << 8;
   return (high | (getbyt() & 255));
}

void newzpc()
{  /*    Newzpc is called whenever the zpc might have crossed a block boundary.
   Normalize the zpc and get new page (if needed).
   */

   zpc1 += (zpc2 >> CVTBLK);   /* normalize (works if negative too) */
   zpc2 &= BYTEBITS;

   if (zpc1 != curblk)
   { /* crossed into new page? */
      curblk = zpc1;      /* update the zpc globals */
      curblkloc = getpag(zpc1);
   }   
}
    
ZIPCHAR *getpag(blk)   /* return a pointer to the requested page */
   short blk;
{
   /*   This is the heart of the paging scheme.  It manages a doubly-linked
   list of block descriptors.  Preloaded pages are not included in this
   list so they cannot be paged out.  If the page requested is preloaded,
   a valid pointer is returned immediately.  

   Otherwise, the block is removed from the linked list, spliced into
   the front of the list and made mru.  There are two subroutines, unlink
   and relink, that manage the linked list.

   If the block is not in core, the current mru's->previous (or lru
   block) buffer is used to page in the requested block.   the
   information in the corresponding block descriptors is filled to
   indicate the absence of the lru and the presence of the new.  The mru
   pointer is  pointed at this block.
   */

   int lru;
   short deadpage;

   if (blk == curpag)       /* same page as last time */
      return(curpagloc);      /* return immediately */

   if (blk < endlod)
   { /* preloaded, expand the pointer */
      curpag = blk;
      curpagloc = Dataspace + ((long)blk << CVTBLK);
      return(curpagloc);
   }   

   if (pagemap[blk] == NOT_IN_CORE)
   {   

/* When choosing the (lru) page to discard, make sure it's not the 
   current code page (where the zpc is), otherwise the newzpc buffer pointer 
   becomes invalid! -- DBB */

      lru = pagedesc[mru].prev;      /* get oldest page */
      deadpage = pagedesc[lru].vpage;
      if (deadpage == curblk)
      { /* but avoid using the zpc page */
         lru = pagedesc[lru].prev;   /* get next oldest page */
         deadpage = pagedesc[lru].vpage;
      }               /* jsf928 */
    
      /* read new page over lru page */
      getblk(blk, pagedesc[lru].loc);  /*jsf928 */
   
      if (deadpage != NO_PAGE)    /* mark old page as gone */
         pagemap[deadpage] = NOT_IN_CORE;
   
      pagemap[blk] = lru;      /* update map for new page */
   
      /* update desc for new page */
      pagedesc[lru].vpage = blk;   /* jsf928 */

      mru = lru;         /* update mru */
   }   
   else            
   { /* page is resident */
      if (pagemap[blk] != mru)  
      { /* if already mru, do nothing */
         unlinkb(blk);         /* unsplice from wherever it is */
         relinkb(blk);         /* link it in as new mru */
      }
   }

   curpag = blk;         /* update page globals */
   curpagloc = pagedesc[mru].loc;   /* jsf928 */
   return(curpagloc);         /* return pointer */
}

void unlinkb(block)
   short block;
{  /*    Unlink removes a block descriptor from the lru chain.
   */
   int t1, t2;

   t1 = pagedesc[pagemap[block]].prev;   /* get pointer to one end */
   t2 = pagedesc[pagemap[block]].next;   /* and the other */
   pagedesc[t1].next = t2;      /* swap pointers */
   pagedesc[t2].prev = t1;
}

void relinkb(block)
   short block;
{  /*  Splice a block back into the lru chain (becomes the new mru).
   */
   int newblk, lru;

   newblk = pagemap[block];      /* "pointer" to the splice block */
   lru = pagedesc[mru].prev;      /* lru "pointer" - jsf928 */
    
   /* update new desc's prev and next */
   pagedesc[newblk].next = mru;   /* jsf928 */
   pagedesc[newblk].prev = lru;   /* jsf928 */
    
   /* update mru and lru descs */
   pagedesc[mru].prev = newblk;   /* jsf928 */
   pagedesc[lru].next = newblk;   /* jsf928 */
    
   mru = newblk;         /* new mru */
}

void bspltb(bytaddr)
   ZIPINT bytaddr;
{  /*   Bspltb takes a (virtual) byte pointer, separates it into byte and 
   block pointers and returns them in the zblk and zoff globals.
   */
   zblk = bytaddr >> CVTBLK;      /* extract block bits */
   zoff = bytaddr & BYTEBITS;      /* extract byte offset bits */
}

void bspltb2(bytaddr)
   long bytaddr;
{  /*   Bspltb takes a (virtual) byte pointer, separates it into byte and 
   block pointers and returns them in the zblk and zoff globals.
   */
   zblk = (short)(bytaddr >> CVTBLK);      /* extract block bits */
   zoff = (short)(bytaddr & BYTEBITS);      /* extract byte offset bits */
}

void bsplit(wrdaddr)
   ZIPINT wrdaddr;
{  /*    Bsplit takes a word aligned pointer, breaks it into a byte and
   block pointer, and returns them in zblk and zoff.
   */
   zblk = wrdaddr >> 8;      /* isolate block bits */
   zoff = (wrdaddr << 1) & BYTEBITS;   /* convert word offset to byte */
}

void bsplitq(quadaddr)
   ZIPINT quadaddr;
{  /*    Bsplitq takes a quad aligned pointer, breaks it into a byte and
   block pointer, and returns them in zblk and zoff.
   */
   zblk = quadaddr >> 7;      /* isolate block bits */
   zoff = (quadaddr << 2) & BYTEBITS;   /* convert word offset to byte */
}
void bsplitq2(quadaddr)
   long quadaddr;
{  /*    Bsplitq takes a quad aligned pointer, breaks it into a byte and
   block pointer, and returns them in zblk and zoff.
   */
   zblk = (short)(quadaddr >> 7);      /* isolate block bits */
   zoff = (short)((quadaddr << 2) & BYTEBITS);   /* convert word offset to byte */
}
void bsplito(octaddr)
   ZIPINT octaddr;
{
    /* Bsplito takes an argument which consists of an oct aligned pointer
       added to either FOFF or SOFF, and breaks it into a byte and
       block pointer, and returns them in zblk and zoff.
   */
   zblk = (short)(octaddr >> 6);
   zoff = (short)((octaddr << 3) & BYTEBITS);
}
void bsplitqy(octaddr)
   long octaddr;
{   /* FOR YZIP'S OCT-ADDRESSING SCHEME */
   zblk = (int)(octaddr >> 9);
   zoff = (int)(octaddr & BYTEBITS);
}

