/************************************************************************
*									*
*	S T A T U S   L I N E   U P D A T E				*
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"

#ifdef GERMAN
#include "german.h"
#endif

#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>

/************************************************************************
*									*
*	S T R I N G   O U T P U T   S U P P O R T 			*
*									*
************************************************************************/

#define RESET_CS  tempcs = permcs	/* reset the character set */

putstr()	/* DECODE A ZSTRING */
{
   /*   String addr given in zblk and zoff, return updated address.

	To understand fully what this procedure does, you must be familiar 
	with zstrings as presented in the ZIP document.  Briefly, a word 
	contains 3 5-bit characters.  There are three character sets, shift 
	chars, and a funny thing called fwords.  Fwords, indicated by the 
	value three (3), are an offset into the table pointed to by wrdtab,
	which contains a pointer to another zstring.  Fword stands for
	Frequently used word.  So this routine is re-entrant.  It is
	guaranteed that an fword will not contain another fword.
   */
    ZIPINT word, bytsav, rword;
    short i, tempcs = 0, permcs = 0;
    char mode = 0, asciflg = 0;
    ZIPINT blk = zblk;
    ZIPINT off = zoff;
    do {
      rword = purwrd(blk,off);	/* get a word, nothing advances */
      off += 2;			/* advance pointer */
      for (i = 1; i <= 3; i++) {
	word = (rword >> (ZCHRLEN * (3 - i))) & ZCHRMSK;
	if (mode == BYTEMODE) THEN
	  if ((word < 6) && (asciflg == 0)) THEN /* special character? */
	    switch (word) {	/* do special case for each */
	      case 0: {
		putchr(SPACE);	/* print a space char */
		RESET_CS;
		break;		/* return to top of loop */
		}
	      case 1:
	      case 2: 		/* calculate fword offset */
	      case 3: {
		word--;		/* use as a multiplicand */
		word <<= 6;	/* blocks are 64 bytes (or 32 words) */
		wrdoff = word;	/* setup offset */
		mode = 1;	/* indicate word mode */
		break;		/* and loop */
		}		/* next pass will invoke a word */
	      case 4:
	      case 5: {		/* handle char set shifts */
		switch (tempcs) {	/* change cs according to current */
		  case 0: {
		      tempcs = word - 3;	/* tempcs becomes 1 or 2 */
		      break;			/* go back to the top */
		      }
		  case 1:	/* cs's 1 + 2 both set permcs */
		  case 2: {
		      if (tempcs != word - 3) THEN	/* not current cs */
			tempcs = 0;		/* reset to 0 */
		      permcs = tempcs;		/* perm too */
		      break;
		      }
		  default: fatal("Undefined shift state - putstr");
		  }				/* end of switch (tempcs) */
		break;
		}				/* end of case 5 */
	      default: fatal("Illegal special code - putstr");
	      }			/* end of switch (word) for special char */
	  else 			/* word is not special char */
	    switch(tempcs) {	/* so process it according to char set */
	      case 0:
	      case 1: {
		putchr(getzchr(word, tempcs));	/* translate char in word */
		RESET_CS;
		break;				/* and go to top of loop */
		}
	      case 2: {				/* see if it is ascii */
		if (asciflg) THEN {
		  asciflg++;			/* byte count */
		  if (asciflg == 2) THEN 
		    bytsav = word << ZCHRLEN;
		  else {
		    word |= bytsav;		/* or in high byte */
		    putchr(word);
		    asciflg = 0;		/* reset ascii mode flag */
		    RESET_CS;
		    }
		  }
		else				/* process non-ascii */
		  switch (word) {	/* cs 2 has specials for 6 & 7 */
		    case 6: {
		      asciflg = 1;	/* set flag to indicate ascii mode */
		      break;
		      }
		    case 7: {
		      newlin();			/* print a crlf */
		      RESET_CS;
		      break;
		      }
		    default: {		/* either a char or ascii */
		      putchr(getzchr(word, tempcs));
		      RESET_CS;
		      }			/* end of default for cs 2 */
		    }			/* end of switch (word) cs 2 */
		  }			/* end of case 2 */
	      }				/* end of switch (tempcs) non-spec */
	else { 				/* we're in WORD mode */
	  word <<= 1;			/* multiply it by 2 */
	  word += wrdtab + wrdoff;	/* index into fword tbl */
	  bsplit(GTVWRD(word));		/* get new ptr */
	  putstr();			/* and print it recursively */
	  mode = 0;			/* turn off word mode */
	  RESET_CS;
	  }				/* end off else word processing */
	}				/* end of for (decoding) loop */
      }					/* end of do loop */
    while ((rword & BIT16) == 0);	/* do until word is negative */
  while (off >= BLKSIZ) {
    blk++;
    off -= BLKSIZ; }
  zoff = off;
  zblk = blk;
}

getzchr(zchr, charset)
ZIPINT zchr, charset;
{  /*	Given a 5 bit code in zchr and a character set number in charset,
	this routine returns the ASCII value of the char.
   */
    charset *= CSETLEN;			/* get offset into char set */
    zchr += charset - 6;		/* add in offset to char */
    return(character_set[zchr]);	/* look it up */
}

/************************************************************************
*									*
*	O U T P U T   B U F F E R I N G					*
*									*
************************************************************************/

putchr(letter)
ZIPINT letter;
{  /*	Putchr takes an ASCII character and queues it for output.  If it
	fills the buffer, a search backwards for a space is conducted to
	break the line.  The end of the buffer is determined by endbuf
	which should be set whenever the screen size is changed to reflect
	the difference between the right and left margins.

        if BUFOUT 0 is active (bufflg=0) then to hell with waiting,
        give the imp what he wants, in otherwords don't wait for
        glory to come to you, go out and get it
   */

/* I think this is the correct place to handle various output
   buffering options.
   The only one I am trying to impliment is TABLE buffer output.
   I also think that this is the only place we need to trap for TABLE 
   buffering. - ASK */

    if (rdir) {                         /* are we sending to a table */
      PTVBYT(rtable2++, letter);       /* letter in table, point to next */
      rdirout++;			/* increment count of characters */
      return;		/* when going to a table we need do nothing else */
      }
    put_to_screen(letter);
    return;
}

static char *last_space = 0;
static int last_space_out = 0;
/* if last_space is set, it points to the buffer location of the most
   recently buffered space.  last_space_out, if 1, means that that space
   has been printed already, so probably not too good to back up to.
   However, if it's -1, then the space was the last thing dumped, so
   we can break by doing a carriage return, without dumping the buffer. */

#ifdef GERMAN
char german_letters[9] = {132, 148, 129, 142, 153, 154, 225, 175, 174};
char *german_strs[9] = {"ae", "oe", "ue", "Ae", "Oe", "Ue", "ss", ">>", "<<"};

cvt_putc(c)
char c;
{
  char *seq;
  if ((c >= LOW_GERMAN) && (c <= HIGH_GERMAN)) {
    if (!language_ok) {
      seq = german_strs[c - LOW_GERMAN];
      while (*seq) {
	md_putc(*seq++); }
      return; }
    c = german_letters[c - LOW_GERMAN]; }
  md_putc(c); }

put_german(letter)
ZIPINT letter;
{
  char *seq;
  seq = german_strs[letter - LOW_GERMAN];
  while (*seq) {
   put_to_screen(*seq++); }
}

#endif

put_to_screen(letter)
ZIPINT letter;
{
    char *tailptr, *dest;
    char brkflg = 0;
    int tempct;
    ZIPINT tletter;
    if ((screen == 1) && (scrx >= scrwid)) THEN
      return;				/* clip output to screen 1 */
#ifdef GERMAN
    if ((letter >= LOW_GERMAN) && (letter <= HIGH_GERMAN)) {
      if (language_ok) {
	letter = german_letters[letter - LOW_GERMAN]; }
       else {
	put_german(letter);
	return; } }
#endif
    if (bufflg == 0) {  		/* give it to them now */
      *chrptr++ = letter;
      if (screen == 0) THEN
	last_space = 0;			/* can't wrap any of this */
      if (letter != Z_EOL) THEN
	*chrptr++ = Z_EOL;
      if ((screen == 0) && vidflg) THEN { /* don't update count if output off */
	chars_on_line++;
	if (letter == SPACE) THEN
	  last_space_out = -1;
	 else if (last_space_out == -1) THEN
	        last_space_out = 1;
	}
      dumpbuf();
      return;  			/* hate to eat and run, but back to work. */
      }
    if (screen == 0) THEN
      chars_on_line++;
    if (screen == 1) THEN {
      if (chrptr == endbuf) THEN {
	tletter = *chrptr;
	*chrptr = Z_EOL;
	dumpbuf();
	*chrptr = tletter; }
      *chrptr++ = letter;	/* buffer the character */
      return;
      }
    if ((chars_on_line < curscrwid) && (chrptr != endbuf)) {
			  		/* if there is room */
      if (letter == SPACE) THEN {
	last_space = chrptr;
	last_space_out = 0; }		/* remember where we can break */
      *chrptr++ = letter; }		/* put the char in the buffer */
    else {
      /* init tail to end of buffer */
      if (letter == SPACE) THEN {
 	newlin();
	return; }			/* just CR if space causes break */
      if (!last_space || last_space_out) { /* no live spaces anywhere */
	if (last_space_out == -1) THEN { /* last thing output was a space */
	  mcrlf();			/* so just CR and start line fresh */
	  cr_int();
	  last_space = 0;
	  last_space_out = 0;
	  chars_on_line = chrptr - outbuf; }
	 else {	  
	  letter = dumpfix(letter, CONSOLE); }
	brkflg = 1; }
       else
	tailptr = last_space;
      if (brkflg == 0) THEN {	/* space found, rearrange buffer */
	tempct = chrptr - tailptr - 1;	/* get count ... */
	*tailptr = NULL;		/* make space into end of line */
	dumpbuf();			/* now print the buffer */
		  				/* and also a CR */
	dest = outbuf;		/* now move remainder of line <-- */
	while (--tempct >= 0) {	/* from space+1 to the beginning */
	  *dest++ = *++tailptr;	/* of the output buffer */
	  chars_on_line++;		/* keep count of chars on line */
	  }
	chrptr = dest;		/* reset the character pointer */
	}				/* end of rearranging of buffer */
      if (letter) THEN {		/* if no crlf */
	chars_on_line++;
	if (letter == SPACE) THEN {
	  last_space = chrptr;
	  last_space_out = 0; }
        *chrptr++ = letter;		/* put the char in the buffer */
	}
      }					/* end of first else */
    return;
}

dumpfix(letter, caller)
int caller;
ZIPINT letter;
{  /*	Dumpfix is called after putchr has searched backwards for a space
	character.  It prints the line and determines when a crlf should
	follow.
        In EZIP/XZIP, don't need second arg, since there is no print buffer 
   */

    dumpbuf();

    if (letter == SPACE) THEN 	/* print a crlf in place of a ' '*/
      return(NULL);		/* NULL is interpreted as crlf */
    else
      return(letter);
}


cr_int()
{
/* do the CRCOUNT, CRFUNC thing */
  int count = GTVWRD(PCRCNT); 		/* get the word		*/
  if (count != 0) {
    PTVWRD(PCRCNT, --count);	/* decrement		*/
    internal_call(GTVWRD(PCRFUNC));   /* do the call          */
    }
}

newlin()
{  /* 	Newlin is called when a CRLF is desired on output of a zstring.
	It flushes the buffer and resets the buffer character pointer.
   */
int count;
    if (rdir) {
      putchr(CR);
/*    putchr(EOL); nuke the lf - ask */
      return; }
    *chrptr++ = NULL;		/* indicate end of line */
    dumpbuf();
    last_space = 0;
}

/* in EZIP there is no print buffering, and scripting is handled
   at the physical character output level */

dumpbuf()
{  /*	Dumpbuf flushes the existing buffer to the screen. */
    if (chrptr != outbuf) THEN
      mprnt(outbuf);		/* this will ignore screen if off */
    if ((screen == 0) && !last_space_out) THEN {
      if (((chrptr - 1) == last_space) ||
	  (((chrptr - 2) == last_space) && (*(chrptr - 1) == Z_EOL))) THEN
	last_space_out = -1;
       else if (last_space < chrptr) THEN
	      last_space_out = 1;
      }
    chrptr = outbuf;		/* reset buffer pointer */
}


print_number(num,prtfunc)
int num;
int (*prtfunc)();
{
  int modulator = 10000;
  if (num == 0) THEN {
    (*prtfunc)('0');
    return; }
  if (num < 0) THEN {
    (*prtfunc)('-');
    num = -num; }
  while (modulator > num)
    modulator /= 10;
  while (ZTRUE) {
    (*prtfunc)('0' + (num / modulator));
    num %= modulator;
    modulator /= 10;
    if (modulator == 0) THEN
      break;
    }
}

errprnt(buf)
char *buf;
{
  int savfont = scrfont;
  int savattr = curattr;
  scrfont = 1;
  curattr = normattr;
  while (*buf) {
#ifdef GERMAN
    cvt_putc(*buf);
#else
    md_putc(*buf);
#endif
    buf++; }
  scrfont = savfont;
  curattr = savattr;
}

gamprnt(buf)
char *buf;
{ /* print a null-terminated string.  No formatting characters allowed! */
  errprnt(buf);
  script_string(buf);
}

script_string(buf)
char *buf;
{
  if (scripting && (screen == 0)) THEN {
    while ((*buf) && (*buf != Z_EOL)) {
      md_script_char(*buf);
      buf++; } }
}

script_input(ibuf)
ZIPINT ibuf;
{
  char chr;
  int count, i;
  if (scripting && (screen == 0)) THEN {
    count = GTVBYT(ibuf + 1) + 2;
    for (i = 2; i <= count; i++) { 
      if (((chr = GTVBYT(ibuf + i)) == NULL) || (chr == Z_EOL))
	break;
      md_script_char(chr); }
  }
}

mprnt(buf)
char *buf;
{  /*	Mprnt prints a string assuming that Z_EOL indicates end of line
	without crlf and null requests a crlf.
	(EZIP.  May have to be modified to support the printing of
	attributes.)
   */
   /* Characters have already been translated for German, so don't
      have to call cvt_putc here. */
    char *sbuf = buf;
    while ((*buf) && (*buf != Z_EOL)) {	/* search for line terminator */
      if (vidflg) THEN
	if ((screen == 0) || (scrx < scrwid)) THEN
          md_putc(*buf);		/* don't print if off margin */
      buf++;				/* advance pointer */
      }					/* print character */
    script_string(sbuf);
    if (*buf == NULL) THEN {
      mcrlf();				/* windowed scroll on ending null */
      cr_int(); }
    return;
}

/* zmprnt is identical to zprnt, but takes a ZIPCHAR *ptr */

zmprnt(buf)
ZIPCHAR *buf;
{  /*	Mprnt prints a string assuming that Z_EOL indicates end of line
	without crlf and null requests a crlf.
	(EZIP.  May have to be modified to support the printing of
	attributes.)
   */
    while ((*buf) && (*buf != Z_EOL)) 	/* search for line terminator */
      md_putc(*buf++);			/* print character */
    if (*buf == NULL) THEN
      mcrlf();				/* windowed scroll on ending null */
    return;
}

chkscript()
{  /* 	Chkscript is called when the flag scrchk has been set.
	Scrchk flag is set in BOR and BAND to indicate that a possible change
	in state of scripting has occurred.  The flag should be set by
	OPDIROUT in EZIP.
   */	
    ZIPINT temp;
    char filename[PATHSIZ];
    int fnl;
    scrchk = 0;				/* reset flag */
    temp = GTVWRD(PFLAGS);		/* get status word */
    if (scripting || scrhld) THEN {	/* set according to current state */
      if ((temp & SCRIPTBIT) == 0) THEN {
	if (scrptfd > 0) THEN		/* not printer */
	  md_close_script(scrptfd);	/* so close the file */
	scripting = 0;			/* and reset flag */
	scrhld = 0;
	floppy_script = 0;
	scrptfd = 0;			/* just to be safe */
	}
      }
     else
      if (temp & SCRIPTBIT) THEN {
        scripting = 1;			/* turn on flag and open */
	gamprnt("Script to (Default is printer, or enter file name): ");
	if (md_getl(filename, PATHSIZ, 0, 0) == 0) THEN {
	  /* use the printer */
	  mcrlf();
	  printabt = 0;
	  if (dosprint) THEN
	    scrptfd = -1;
	   else {
	    if (scrptfd = md_find_printer()) THEN
	      scrptfd = -scrptfd;
	     else {
	      scrptfd = 0;
	      script_off(); } }
	  }
	 else {
	  mcrlf();
	  floppy_script = 0;
	  md_check_swap(filename);
	  if (swapped && swap_to_floppy && (!no_paging)) {
	    gamprnt("Can't script to another floppy on one-floppy system.");
	    mcrlf();
	    script_off();
	    swapped = 0;
	    if (gamechn == 0) 
	      md_get_game_chan(0);
	    PTVWRD(PFLAGS, GTVWRD(PFLAGS) ^ FSTAT);
	    return; }
	  if (swapped)
	    floppy_script = get_fname_drive(filename) + 1;
	  swapped = 0;
	  if (gamechn == 0)
	    md_get_game_chan(0);
	  if ((scrptfd = zopen(filename, 1)) >= 0) {
	    do_seek(scrptfd, 2, 0);
	    gamprnt("Appending to existing file.");
	    mcrlf(); }
	  else if ((scrptfd = zcreat(filename)) < 0) {
	    scrptfd = 0;
	    script_off();
	    gamprnt("Couldn't open script file.");
	    mcrlf(); }
        } }
    return;
}

flush_buffer()
{
  if (chrptr != outbuf) THEN {
    put_to_screen(Z_EOL);		/* skip possible table output... */
    dumpbuf();
    }
}

not_terminator(c)
char c;
{  ZIPINT trmtab;
   char ctchar;
   trmtab = GTVWRD(PTCHARS);
   if (trmtab != 0) THEN {
     bspltb(trmtab);
     while ((ctchar = getbyt()) != 0) {
       if (c == ctchar) THEN
	 return(ZFALSE);
       if ((c >= 128) && (ctchar == 255)) THEN
	 return(ZFALSE);
	/* handle 255, meaning all function keys terminate */
       }
     }
   return(ZTRUE);
}

script_off ()
{
  scripting = 0;
  if (scrptfd > 0) THEN
    zclose(scrptfd);
  scrptfd = 0;
  floppy_script = 0;
  PTVWRD(PFLAGS, GTVWRD(PFLAGS) & (~SCRIPTBIT));
}
