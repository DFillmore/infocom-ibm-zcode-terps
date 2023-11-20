/************************************************************************
*									*
*	G A M E   C O M M A N D S					*
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"
#include <dos.h>

internal_sav_res(fcn)
int fcn;
{  /* save/restore to memory, if possible.  Save function will never
      be called unless we think there's enough memory to do it.  The
      restore function will never be called unless a save has already
      happened.  The buffer is allocated during initialization. */
   ZIPCHAR *tbuf;
   ZIPINT scrsave[7];
   if (isave_buffer) THEN {
     /* we have the memory for this */
     save_screen_stuff(&scrsave[0]);
     push_save_stuff();
     if (fcn == OPSAVE) THEN
       do_copy(0, zstack, FP_SEG(isave_buffer), FP_OFF(isave_buffer),
	      (LSTACK * 2));	/* save the stack */
      else
       do_copy(FP_SEG(isave_buffer), FP_OFF(isave_buffer), 0, zstack,
	      (LSTACK * 2));	/* restore the stack */
     if (pop_save_stuff ()) THEN {
       if (fcn == OPSAVE) THEN {
	 tbuf = isave_buffer + (LSTACK * 2);
	 /* save impure space */
	 do_copy(FP_SEG(dataspace), FP_OFF(dataspace), FP_SEG(tbuf),
		FP_OFF(tbuf), GTVWRD(PPURBT));
	 isave_happened = 1;
	 restore_screen_stuff(&scrsave[0]);
	 return(1); }
	else {
	 /* restore impure space */
	 tbuf = isave_buffer + (LSTACK * 2);
	 do_copy(FP_SEG(tbuf), FP_OFF(tbuf),
		FP_SEG(dataspace), FP_OFF(dataspace), GTVWRD(PPURBT));
	 restore_screen_stuff(scrsave);
	 reset_screen();
	 return(2); }
       }
      else
       fatal("IRESTORE failed");
    }
}

save_screen_stuff(scrsave)
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

restore_screen_stuff(scrsave)
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

reset_screen()
{
  LM = GTVWRD(PLMRG) / fontsize;
  RM = GTVWRD(PRMRG) / fontsize;
  curscrwid = scrwid - LM - RM;
}

push_save_stuff()
{
  PUSHZ(zpc1);			/* save important stuff */
  PUSHZ(zpc2);
  PUSHZ(zlocs);
  PUSHZ(zargct);
  PUSHZ(zorkid);
  zstack[0] = zsp - zstack;	/* relative stack pointer */
}

pop_save_stuff()
{
  zsp = zstack + zstack[0];	/* absolute stack pointer */
  if (*zsp == zorkid) THEN {
    POPZ();			/* zorkid */
    zargct = POPZ();
    zlocs = POPZ();
    zpc2 = POPZ() & BYTEBITS;
    zpc1 = POPZ();
    newzpc();
    return(1); }
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
    char *fptr, filename[PATHSIZ], *s, *d,
         *entstr = "Enter save file name."; 
    char chr;
    int j = 0, errcode, namlen, i;
    ZIPINT oldflags;

    flush_buffer();
    md_hlite(0);
    gamprnt(entstr);		/* print enter string */
    mcrlf();				/* windowed scroll */
    gamprnt("(Default is ");	/* print last save file */

/* make savfile = name  as default */
    bspltb(name);
    namlen = getbyt();
    for (i = 1; i <= namlen; i++) {
    	chr = getbyt();
	savfile[j++] = chr; }
    savfile[j] = 0;
    md_display_default(savfile, filename);
    gamprnt("): ");
    if (md_getl(filename, PATHSIZ, 0, 0) == 0) THEN /* get a line of input */
      fptr = savfile;				/* use default on crlf */
    else 
      fptr = filename; 			/* otherwise use entered name */
    if (md_parse_file(fptr,filename,savfile)) {
      mcrlf();
      gamprnt("Using ");
      gamprnt(filename);
      gamprnt(".");
      }
     else if (scripting)		/* script save file name if nec */
      script_string(fptr);
    fptr = filename;
    mcrlf();
    md_check_swap(fptr);
    if (fcn == OPSAVE) 			/* create or open accordingly */
      savechn = zcreat(fptr);
    else
      savechn = zopen(fptr, RDONLY);
    if (savechn < 0) {
      gamprnt("Can't find/create file.");
      mcrlf();
      md_check_unswap();
      return(ZFALSE); }
    if (fcn == OPSAVE) {		/* r/w name */
      errcode=(far_write(savechn,0,&namlen,sizeof(namlen)) != sizeof(namlen));
      if (!errcode && !diskabt) THEN
	errcode = (far_write(savechn, 0, savfile, namlen) != namlen); }
     else {
      errcode = (far_read(savechn, 0, &j, sizeof(j)) != sizeof(j));
      if (!errcode && !diskabt) THEN {
	if (j > OBUFSIZ) THEN
	  j = OBUFSIZ;
	errcode = (far_read(savechn, 0, outbuf, j) != j); } }
    if (errcode || diskabt)  {
      zclose(savechn);
      if (fcn == OPSAVE) {
	gamprnt("Can't write file.");
	if (!diskabt) gamprnt(" Disk full?");
	do_delete(fptr); }
       else
        gamprnt("Can't read file.");
      mcrlf();
      md_check_unswap();
      return(ZFALSE); }
    if (fcn == OPSAVE) THEN
      errcode = wrtbyts(dataspace + start, length);
     else {
      if ((j != namlen) ||
	  !same_name(outbuf, savfile, j)) {
	md_sound(FEEP);
	gamprnt("Wrong save file. RESTORE aborted.");
	mcrlf();
	md_check_unswap();
	return(ZFALSE); }
      errcode = rdbyts(dataspace + start, length); }
    zclose(savechn);
    if ((errcode == ZFALSE) || diskabt) THEN {
      if (fcn == OPREST) THEN
	fatal("Partial read on restore.");
       else {
	gamprnt("Can't write file.");
	if (!diskabt) gamprnt(" Disk full?");
	do_delete(fptr);
	md_check_unswap();
	mcrlf(); }
      return(ZFALSE); }
    md_check_unswap();
    md_install_default(fptr,savfile);	/* save the name used... */
    if (fcn == OPREST) THEN
      return(length);
     else
      return(ZTRUE);		/* return success */
}

same_name(buf, file, len)
int len;
char *file, *buf;
{ 
   int i;
   for (i = 0; i < len; i++) {
     if (buf[i] != file[i]) THEN return(0); }
   return(1);
}
   
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
    char *fptr, filename[PATHSIZ], *s, *d, *nptr,
         *entstr = "Enter save file name."; 
    ZIPINT scrsave[7];
    int i, j, errcode;
    flush_buffer();
    md_hlite(0);
    gamprnt(entstr);		/* print enter string */
    mcrlf();				/* windowed scroll */
    gamprnt("(Default is ");	/* print last save file */
    md_display_default(savfile, filename);
    gamprnt("): ");
    if (md_getl(filename, PATHSIZ, 0, 0) == 0) THEN /* get a line of input */
      fptr = savfile;				/* use default on crlf */
    else
      fptr = filename; 			/* otherwise use entered name */
    if (md_parse_file(fptr, filename, savfile)) {
      mcrlf();
      gamprnt("Using ");
      gamprnt(filename);
      gamprnt("."); }
     else if (scripting) THEN		/* script save file name if nec */
      script_string(fptr);
    fptr = filename;
    mcrlf();
    md_check_swap(fptr);
    if (fcn == OPSAVE) THEN		/* create or open accordingly */
      savechn = zcreat(fptr);
    else
      savechn = zopen(fptr, RDONLY);
    if (savechn >= 0) THEN {		/* if sucessful, save stack */
      save_screen_stuff(&scrsave[0]);
      push_save_stuff();			/* save vitals */
      if (fcn == OPSAVE)		/* r/w stack */
	errcode = (far_write(savechn, 0, zstack, LSTACK*2) == (LSTACK * 2));
       else
	errcode = (far_read(savechn, 0, zstack, LSTACK*2) == (LSTACK * 2));
      if (!diskabt && (errcode != ZFALSE)) THEN {	/* read succeeded */
        if (pop_save_stuff ()) THEN {	/* will check zorkid */
	  if (fcn == OPSAVE) THEN {	/* r/w impure code accordingly */
	    errcode = wrtbyts(dataspace, GTVWRD(PPURBT));
	    }
	   else {
	    errcode = rdbyts(dataspace, GTVWRD(PPURBT));
	    }
	  restore_screen_stuff(&scrsave[0]);
	  reset_screen();
	  zclose(savechn);			/* close the file */
	  if ((errcode != ZFALSE) && !diskabt) THEN {
	    md_install_default(fptr,savfile);
	    md_check_unswap();
	    if (fcn == OPREST) THEN
	      return(2);
	     else
	      return(ZTRUE);		/* return success */
	    }
	   else
	    if (fcn == OPREST) THEN 	/* restore failure is fatal */
	      fatal("Partial read on restore");
	     else {
	      do_delete(fptr);
	      gamprnt("Couldn't write save.");
	      if (!diskabt) gamprnt(" Disk full?");
	      diskabt = 1;
	      mcrlf(); }
	}
       else
	fatal("Wrong game or version");		/* bad zorkid */
      }
     else
      for (i = 1; i <= 5; i++) POPZ();	/* flush vitals */
     }					/* end of if savechn */
    else {
      if (fcn == OPSAVE) {
	gamprnt("Couldn't create save file.");
	mcrlf();
	md_check_unswap();
	return(ZFALSE); }
      gamprnt("Invalid save file");
      mcrlf();
      }
    if ((fcn == OPSAVE) && (!diskabt)) {
      do_delete(fptr);
      gamprnt("Couldn't write save. Disk full?");
      mcrlf(); }
    md_check_unswap();
    return(ZFALSE);
}

do_delete(str)
char *str;
{
  union REGS regs;
  regs.h.ah = 0x41;
  regs.x.dx = str;
  int86(0x21, &regs, &regs);
}

verify()
{  /*   Verify computes a checksum on the entire data file, less the header.
	All pages are brought in from disk.  The checksum is then compared 
	to the checksum stored in the header. (EZIP - Remove annoucing printf)
   */
    ZIPBYT buffer[BLKSIZ];		/* buffer to use */
    ZIPINT chksum = 0, blocksum();
    short i, lastblk, lastoff;
    bsplitq(GTVWRD(PLENTH));	/* get length of game file */
    lastblk = zblk;
    lastoff = zoff;

    chksum += blocksum(buffer, 0, HDRSIZ, BLKSIZ); /* skip the header bytes */
    for (i=1; i<lastblk; i++)
      chksum += blocksum(buffer, i, 0, BLKSIZ);
    chksum += blocksum(buffer, lastblk, 0, lastoff);/* sum the final bytes */

    if (chksum == GTVWRD(PCHKSM)) THEN		/* desired checksum */
      return(ZTRUE);
    else 
      return(ZFALSE);
}

ZIPINT blocksum (buffer, block, off1, off2)	/* checksum a block */
ZIPBYT buffer[BLKSIZ];
short block, off1, off2;
{
    register ZIPINT sum = 0, i;

    lseek(gamechn, (long)block << CVTBLK, 0);	/* seek to block */
    if ((far_read(gamechn, 0, buffer, off2) != off2) || diskabt) THEN
      fatal("Read block error");
    for (i=off1; i<off2; i++)		/* sum between given offsets */
      sum += buffer[i];
    return(sum);
}

/*  FORMER VERIFY HACK, USED GETBYT(), NOW DEAD.

    Setting endlod to 0 tells the paging routines that there's no preload,
    i.e., search for each page in the page buffers.  If it's not there
    (as preload pages normally would not be), then it's paged in from disk.
*/

