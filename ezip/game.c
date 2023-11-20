/************************************************************************
*									*
*	G A M E   C O M M A N D S					*
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"

sav_res(fcn)
int fcn;
{  /*	Save and restore routines.  This routine receives the OP as a
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
    char *fptr, filename[PATHSIZ], *s, *d,
         *entstr = "Enter save file name."; 
    int i, errcode;
    ZIPINT oldflags;

    printf("%s", entstr);		/* print enter string */
    mcrlf();				/* windowed scroll */
    printf("(Default is %s): ", savfile);	/* print last save file */
    if (md_getl(filename, PATHSIZ, 0, 0) == 0) THEN /* get a line of input */
      fptr = savfile;				/* use default on crlf */
    else
      fptr = filename; 			/* otherwise use entered name */
    if (scripting) THEN			/* script save file name if nec */
      fprintf(scrptfd, "%s\n(Default is %s): %s\n", entstr, savfile, fptr);
    if (fcn == OPSAVE) THEN		/* create or open accordingly */
      savechn = creat(fptr, FMODE);
    else
      savechn = open(fptr, RDONLY);
    if (savechn != -1) THEN {		/* if sucessful, save stack */
      PUSHZ(zpc1);			/* save vitals */
      PUSHZ(zpc2);
      PUSHZ(zlocs);
      PUSHZ(zorkid);
      zstack[0] = zsp - zstack;		/* relativize stack pointer */
      if (fcn == OPSAVE)		/* r/w stack */
	errcode = wrtbyts(zstack, LSTACK*2);
      else
	errcode = rdbyts(zstack, LSTACK*2);
     zsp = zstack + zstack[0];		/* unrelativize stack pointer */
     if (*zsp == zorkid) THEN 		/* check version */
      if (errcode != ZFALSE) THEN {
	POPZ();				/* throw away copy of zorkid */
	zlocs = POPZ();			/* restore vitals */
	zpc2 = POPZ() & BYTEBITS;	
	zpc1 = POPZ();
	if (fcn == OPSAVE) THEN		/* r/w impure code accordingly */
	  errcode = wrtbyts(dataspace, GTVWRD(PPURBT));
	else {
	  oldflags = GTVWRD(PFLAGS);
	  errcode = rdbyts(dataspace, GTVWRD(PPURBT));
	  PTVWRD(PFLAGS, oldflags);	/* preserve script status flag */
	  }
	close(savechn);			/* close the file */
	if (errcode != ZFALSE) THEN {
	  s = fptr;			/* save the save file name */
	  d = savfile;
	  while (*d++ = *s++);
	  newzpc ();
	  return(ZTRUE);		/* return success */
	  }
	else
	  if (fcn == OPREST) THEN 	/* restore failure is fatal */
	    fatal("Partial read on restore");
	}				/* end of if errcode <> -1 */
      else
	for (i = 1; i <= 4; i++) POPZ();	/* flush vitals */
     else
       fatal("Wrong game or version");		/* zorkid's didn't match */
     }					/* end of if savechn */
    else {
      printf("Invalid save file");
      mcrlf();
      if (scripting) THEN
	fprintf(scrptfd, "Invalid save file\n");
      }
    return(ZFALSE);
}

verify()
{  /*   Verify computes a checksum on the entire data file, less the header.
	All pages are brought in from disk.  The checksum is then compared 
	to the checksum stored in the header. (EZIP - Remove annoucing printf)
   */
    ZIPINT chksum = 0, blocksum();
    short i, lastblk, lastoff;

    printf("Unix Interpreter Version A");	/* version */
    mcrlf();					/* windowed scroll */
#ifdef ZIP
    bsplit(GTVWRD(PLENTH));	/* get length of game file */
#endif
#ifdef EXZIP
    bsplitq(GTVWRD(PLENTH));	/* get length of game file */
#endif
    lastblk = zblk;
    lastoff = zoff;

    chksum += blocksum(0, HDRSIZ, BLKSIZ);	/* skip the header bytes */
    for (i=1; i<lastblk; i++)
      chksum += blocksum(i, 0, BLKSIZ);
    chksum += blocksum(lastblk, 0, lastoff);	/* sum the final bytes */

    if (chksum == GTVWRD(PCHKSM)) THEN		/* desired checksum */
      return(ZTRUE);
    else 
      return(ZFALSE);
}

ZIPINT blocksum (block, off1, off2)	/* checksum a block */
short block, off1, off2;
{
    ZIPBYT buffer[BLKSIZ];
    register ZIPINT sum = 0, i;

    getblk(block, buffer);		/* read block from disk */
    for (i=off1; i<off2; i++)		/* sum between given offsets */
      sum += buffer[i];
    return(sum);
}

/*  FORMER VERIFY HACK, USED GETBYT(), NOW DEAD.

    Setting endlod to 0 tells the paging routines that there's no preload,
    i.e., search for each page in the page buffers.  If it's not there
    (as preload pages normally would not be), then it's paged in from disk.
*/

