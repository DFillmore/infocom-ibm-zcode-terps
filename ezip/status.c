/************************************************************************
*									*
*	S T A T U S   L I N E   U P D A T E				*
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"

#ifdef ZIP

statusln()
{  /*	Status line updates the status line.  It finds the current room
	description, time, score and moves and displays them.  The first
	global variable in ZIL is called here and in the object number where
	the player is.  Printd for this object will print the room description.
	The next two globals are either time or score and moves.  The timemd
	variable set during ZIPINI determines the type of status line to 
	use.  This routine will have to modified to support a windowed 
	environment.
	(EZIP.  Please scrap this routine for ezip.)
   */
    char *chrsav, line[81], *meridian = "am";
    ZIPINT score, moves, hour, minutes;
    int i;

    for (i = 0; i < 80; i++) line[i] = SPACE;	/* clean out line */
    line[80] = NULL;			/* end char */
    chrsav = chrptr;			/* save ptr for out buf */
    chrptr = &line[1];			/* indent one space in string */
    PUSH(scripting);			/* save state of this */
    scripting = 0;			/* turn off scripting */
    printd(getvar(G_HERE));		/* print room's short desc */
    chrptr = chrsav;			/* restore char pointer */
    scripting = POP();			/* restore state of scripting */
    if (timemd) THEN {			/* just print time */
      hour = getvar(G_HOURS);		/* get hours passed */
      if (hour >= 12) THEN
	*meridian = 'p';		/* make it pm */
	if (hour > 12) THEN
	  hour -= 12;			/* round off */
      minutes = getvar(G_MINS);		/* get minutes */
      sprintf(&line[60],"Time:  %1d:%02.2d %s", hour, minutes, meridian);
      }
    else {				/* do score / moves status */
      score = getvar(G_SCORE);		/* get current score */
      moves = getvar(G_MOVES); 		/* get current number of moves */
      sprintf(&line[50],"Score: %1d", score);
      sprintf(&line[66],"Moves: %1d", moves);
      }
    for (i = 0; i < 80; i++) 
      if (line[i] == NULL) THEN
	line[i] = SPACE;		/* remove nulls from sprintf */
    locate(STATLEN, 1);			/* go to upper left */
    md_hilite(REVERSE);			/* turn on reverse video */
    printf("%s", line);			/* print the status line */
    md_hilite(NORMAL);			/* restore normal video */
    locate(25,1);
    return;
}

#endif	/* ZIP */

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
    ZIPINT word, bytsav, oldblk, oldoff;
    short i, tempcs = 0, permcs = 0;
    char mode = 0, asciflg = 0;

    do {
      word = getwrd();		/* get next word, advance zblk and zoff */
      PUSH(word);		/* save word and pointer */
      for (i = 1; i <= 3; i++) {	/* three bytes to a word */
        PUSH(word);		/* save current low order bits */
        word >>= ZCHRLEN;	/* shift down to next byte */
        }
      for (i = 1; i <= 3; i++) {
        word = POP();		/* get next byte */
        word &= ZCHRMSK;	/* isolate byte bits */
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
	  oldblk = zblk;		/* re-entering, save current globals */
	  oldoff = zoff;
	  bsplit(GTVWRD(word));		/* get new ptr */
	  putstr();			/* and print it recursively */
	  zblk = oldblk;
	  zoff = oldoff;
	  mode = 0;			/* turn off word mode */
	  RESET_CS;
	  }				/* end off else word processing */
	}				/* end of for (decoding) loop */
      word = POP();			/* get next character */
      }					/* end of do loop */
    while ((word & BIT16) == 0);	/* do until word is negative */
}

getzchr(zchr, charset)
ZIPINT zchr, charset;
{  /*	Given a 5 bit code in zchr and a character set number in charset,
	this routine returns the ASCII value of the char.
   */
    charset *= CSETLEN;			/* get offset into char set */
    zchr += charset - 6;		/* add in offset to char */
    return(*(zchars+zchr));		/* lookup the character */
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
    char *tailptr, *dest;
    char brkflg = 0;
    int tempct;
#ifdef ZIP   
    if (scripting) THEN
      p_putchr(letter); 
    PUSH(scripting);			/* save state of scripting */
    scripting = 0;			/* turn off for duration */
#endif /* ZIP */

#ifdef EXZIP
/* I think this is the correct place to handle various output
   buffering options.
   The only one I am trying to impliment is TABLE buffer output.
   I also think that this is the only place we need to trap for TABLE 
   buffering. - ASK */

    if (rdir) {                         /* are we sending to a table */
      PTVBYT(rtable2++, letter);        /* put letter in the table and point to next position */
      rdirout++;			/* increment count of characters */
      return;				/* when going to a table we need do nothing else */
      }
#endif

    if (bufflg == 0) {  		/* give it to them now */
      *chrptr++ = letter;
      *chrptr++ = Z_EOL;
      if (screen == 0) THEN
	chars_on_line++;
      dumpbuf();
      return;        			/* hate to eat and run, but back to work. */
      }
    if ((screen == 0) && vidflg) THEN
      chars_on_line++;
    if ((chars_on_line < scrwid) && (chrptr != endbuf))
			  		/* if there is room */
      *chrptr++ = letter;		/* put the char in the buffer */
    else {
      /* init tail to end of buffer */
      tailptr = chrptr;			/* point just past last char in buf */
      while (*--tailptr != SPACE) {	/* search backwards for a " " */
	if (tailptr == outbuf) THEN {	/* not a space to be found! */
	  letter = dumpfix(letter, CONSOLE); /* dump buffer and add crlf */
	  brkflg = 1;			/* indicate reason for loop exit */
	  break;
	  }				/* end of while searching for ' '*/
	}
      if (*tailptr == SPACE) THEN 	/* space found, rearrange buffer */
	if ((tailptr == outbuf) && (brkflg != 1)) THEN
	  letter = dumpfix(letter, CONSOLE);
	else {
	  tempct = chrptr - tailptr - 1;
	  *tailptr = NULL;		/* make space into end of line */
	  dumpbuf();			/* now print the buffer */
	  				/* and also a CR */
	  dest = outbuf;		/* now move remainder of line <-- */
	  while (--tempct >= 0) {	/* from space+1 to the beginning */
	    *dest++ = *++tailptr;	/* of the output buffer */
	    if (vidflg) THEN
		chars_on_line++;	/* keep count of chars on line */
	    }
	  chrptr = dest;		/* reset the character pointer */
	  }				/* end of rearranging of buffer */
      if (letter) THEN {		/* if no crlf */
	if (vidflg) THEN
	  chars_on_line++;
        *chrptr++ = letter;		/* put the char in the buffer */
	}
      }					/* end of first else */
#ifdef ZIP
    scripting = POP();			/* restore scripting state */
#endif
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

#ifdef ZIP		/* CONSOLE only in EZIP, CONSOLE | PIPE in ZIP */
    if (caller == CONSOLE) THEN
      dumpbuf();		/* print the line */
    else
      dmp_pbuf();		/* print the line on pipe */
#else				/* no print buffering in EXZIP */
    dumpbuf();
#endif

    if (letter == SPACE) THEN 	/* print a crlf in place of a ' '*/
      return(NULL);		/* NULL is interpreted as crlf */
    else
      return(letter);
}

#ifdef ZIP

p_putchr(letter)		/* put char to pipe (usually printer) */
ZIPINT letter;
{  /* 	P_putchr is a fixed width version of putchr for writing to a pipe
	(for scripting).  It function is the same with the exception that
	p_endbuf is fixed at 79.
   */
    char *tailptr, *dest;
    char brkflg = 0;
   
    if (p_chrptr != p_endbuf) THEN 	/* if there is room */
      *p_chrptr++ = letter;		/* put the char in the buffer */
    else {
      tailptr = p_endbuf;		/* init tail to end of buffer */
      while (*--tailptr != SPACE) {	/* search backwards for a " " */
	if (tailptr == p_outbuf) THEN {	/* not a space to be found! */
	  letter = dumpfix(letter, PIPE);    /* dump buffer and add crlf */
	  brkflg = 1;			/* indicate reason for loop exit */
	  break;
	  }				/* end of while searching for ' '*/
	}
      if (*tailptr == SPACE) THEN 	/* space found, rearrange buffer */
	if ((tailptr == p_outbuf) && (brkflg != 1)) THEN
	  letter = dumpfix(letter, PIPE);
	else {
	  *tailptr = NULL;		/* make space into end of line */
	  dmp_pbuf();			/* now print the buffer */
	  dest = p_outbuf;		/* now move remainder of line <-- */
	  while (++tailptr < p_endbuf) 	/* from space+1 to the beginning */
	    *dest++ = *tailptr;		/* of the output buffer */
	  p_chrptr = dest;		/* reset the character pointer */
	  }				/* end of rearranging of buffer */
      if (letter) THEN			/* if no crlf */
        *p_chrptr++ = letter;		/* put the char in the buffer */
      }					/* end of first else */
    return;
}

#endif	/* ZIP */

newlin()
{  /* 	Newlin is called when a CRLF is desired on output of a zstring.
	It flushes the buffer and resets the buffer character pointer.
   */
    *chrptr = NULL;		/* indicate end of line */
#ifdef ZIP
    if (scripting) THEN
      *p_chrptr = NULL;
#endif
    dumpbuf();
}

/* in EZIP there is no print buffering, and scripting is handled
   at the physical character output level */

dumpbuf()
{  /*	Dumpbuf flushes the existing buffer to the screen. */
#ifdef ZIP
    if (scripting) THEN {
      dmp_pbuf();
      p_chrptr = p_outbuf;
      }
#endif
    if (vidflg) THEN
      mprnt(outbuf);
    chrptr = outbuf;		/* reset buffer pointer */
}

#ifdef ZIP

dmp_pbuf()
{  /* 	Dmp_pbuf flushes the existing output buffer for the pipe to the
	pipe.
   */
    char *bufptr;

    bufptr = p_outbuf;			/* get another pointer */
    while ((*bufptr) && (*bufptr != Z_EOL))
      bufptr++;				/* search for end of string */
    if (*bufptr == Z_EOL) THEN {	/* drop in end char */
      *bufptr = NULL;
      fprintf(scrptfd, "%s", p_outbuf);	/* write string */
      }
    else
      fprintf(scrptfd, "%s\n",p_outbuf);/* with crlf if necessary */
}

#endif /* ZIP */

mprnt(buf)
char *buf;
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

    scrchk = 0;				/* reset flag */
    temp = GTVWRD(PFLAGS);		/* get status word */
    if (scripting) THEN {		/* set according to current state */
      if ((temp & SCRIPTBIT) == 0) THEN {
	fclose(scrptfd);		/* close if turned off */
	scripting = 0;			/* and reset flag */
	}
      }
    else
      if (temp & SCRIPTBIT) THEN {
        scripting = 1;			/* turn on flag and open */
	p_chrptr = p_outbuf;		/* reset scripting buffer */
        if ((scrptfd = fopen("script", "w")) < 0) THEN 
	  scripting = 0;		/* turn off flag if open fails */
        }
    return;
}

flush_buffer()
{
  putchr(Z_EOL);
  dumpbuf();
}