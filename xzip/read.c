/************************************************************************
*									*
*	R E A D   A N D   S U P P O R T I N G   R O U T I N E S 	*
* 									*
*       ALSO HAS MACHINE INDEPENDENT MOUSE ROUTINES                     *
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"
#include "ctype.h"	/* for islower() */

ZIPINT lookup();
char chrbyt();
short numread;

int zread()
{  /*	Read is probably the most complex zip opcode.  It not only reads
	a line of input but it also performs some basic parsing and table
	lookup.  The arguments to Read are an input buffer and a token
	table to store the results of the lookups.  There may be two extra
	arguments, the first being a timeout time (in 10ths of a second),
	the second being the routine to call when the timeout is invoked.
	If the routine returns non-zero, the read exits, without calling
	LEX or doing anything silly.  It's up to the user to figure out
	that the read didn't actually happen, probably by setting a global
	in the timeout routine.
	The input buffer's
	first byte is a read-only byte to indicate the maximum number
	of input characters.  The first two bytes of the token table are
	also special.  The first is supposed to be the maximum number of
	tokens.  Unfortunately it always puts twice that number in the
	first slot.  This problem has been an ongoing argument between
	(technical) imps and the micro group.  The micro group finally
	won, since the compiler got fixed, so this is no longer a problem.
	he second byte
	of the token table is set by the interpreter (us) to indicate the
	number of tokens actually parsed.  Tokens begin at the third byte
	(offset 2) and are 4 bytes each.  The format of each is as follows:

		2 bytes -- pointer to word in vocabulary table 
		1 byte  -- length of ascii input
		1 byte  -- offset into input buffer for corresponding text.
	
	If the token buffer is filled, any other input is flushed.

	The input buffer is only changed in that all input should be
	lowercasified.  If it is filled, it beeps.

	Read has a number of subroutines for processing the input string.
	The status line is always updated before taking input.  There are
	a number of global pointers for read declared for use by many of
	the subroutines.  See the variable declarations for Read for an
	explanation of each.

	The basic flow is as follows: A line is read in and lowercasified.
	Getasc gets a single word and determines its length.  Nxtasc finds
	the next token to be used.  Zword is used to convert an ascii word
	to zstring format.  The zword (two 16 bit words in zip) is then
	looked up in the vocabulary table.  A token value of zero is stored
	if no match is found.  The process continues until the token table
	is filled or all the ascii input has been processed.

	Keep in mind the concept of a self-inserting break character.  These
	characters, such as "period" and "comma," are treated as words,
	converted to zwords and looked up in the vocab table.  SI breaks
	are initially downloaded from the datafile during initialization in
	ZIPINI.  

	(EZIP.  The basic read routine will be unchanged.  Support for
	incall, timeout, and input will have to be added.  Statusln will
	be removed.  Timeout routines must be built into getlin which
	will require a special version of md_getl.)

	(XZIP.  Split into component parts, so user can convert bytes to
	zwords, and invoke LEX.  LEXV argument is optional.  etc.)
   */
    short argct=argblk[0];
    ZIPINT inbuf=argblk[1];
    ZIPINT lexv=argblk[2];
    short timeout=argblk[3];
    short timeroutine=argblk[4];

    if (argct < 2)
      lexv = 0;				/* default the second arg */
    flush_buffer();			/* flush output */
    if (argct == 4)			/* do call with timeout args */
        numread = getlin(inbuf,timeout,timeroutine);
      else				/* do call with no timeout */
       numread = getlin(inbuf,0,0);
    if (numread < 0) 
      return;				/* means getlin aborted */
    PTVBYT(inbuf+1,numread);		/* store number read in inbuf */
    /* write to script if last char was CR, and if scripting */
    if (scripting && ((read_terminator == CR) || (read_terminator == EOL))){
      if (numread > 0) 
	script_input(inbuf);
      }
    if (read_terminator == CR)
      mcrlf();				/* this will also script */
    if (lexv) 
      zlex(inbuf,lexv,ZFALSE,ZFALSE);	/* call lex primitive if needed */
    return(0xff & (short)read_terminator);    /* return the right thing */
}

zlex(inbuf,lexv,lexicon,preserve)
ZIPINT inbuf,lexv,lexicon,preserve;
{
    short rdnwds,maxtoks,wordlen,temp;
    ZIPINT rdret, wordent;
    numread = GTVBYT(inbuf+1);		/* number of characters in buffer */
    curoff = 2;				/* first real thing in buffer */
    numread += curoff;			/* so they have same base */
    if (lexicon == 0)
      lexicon = main_vocabulary;	/* default lexicon argument */
    rdret = lexv + 1;		/* table 2 token count */
    if (numread == curoff) {
      PTVBYT(rdret,0);
      return;
      } 
    rdnwds = 0;				/* no words parsed, ...yet */
    maxtoks = GTVBYT(lexv);		/* max tokens in input buffer */
    wordent = lexv + 2; 		/* words begin after count bytes */
    rdbos = (ZIPCHAR *)(inbuf + dataspace);
    while (ZTRUE) {
      /* first munch leading breaks.  stop at a self-inserting break,
	 of course. */
      while ((curoff < numread) && (notbrkc(rdbos[curoff]) == ZFALSE)) {
        if (lastbrk < esibrks) 
	  break;			/* but it was self-inserting-->stop */
	curoff++;			/* get to beginning of a word */
	}
      if ((curoff >= numread) || (rdnwds > maxtoks)) THEN
        break;				/* out of stuff or room */
      wordlen = zwstr(inbuf,0,curoff,0);	/* get word into table */
      PTVBYT(wordent+2, wordlen);	/* fill in length */
      PTVBYT(wordent+3, curoff);	/* and start */
      if (temp = lookup(lexicon))	/* do lookup */
	PTVWRD(wordent,temp);		/* succeeded */
       else
	if (!preserve) 			/* don't clobber if preserve true */
	  PTVWRD(wordent,0);
      rdnwds++;				/* increment number of words read */
      wordent += 4;			/* skip to next word entry */
      curoff += wordlen;		/* update curoff */
      }					/* end of while (ZTRUE) */
    if (curoff < numread)
      flushwrds(rdbos, curoff, numread);
    PTVBYT(rdret, rdnwds);
    return;
}

/* call this to convert a word in an input buffer into a zword.  The
   first arg is the buffer, a table (assumed writable).  The second
   arg is the length of the word, currently ignored.  The third is the
   offset of the first character to be converted.  The fourth is the table
   to stuff the result in, or 0.  If 0, this is an internal call, and
   the result will go in hi/lo/third.  The first character is assumed to
   not be a break.  Or to be a self-inserting break. */
int zwstr(inbuf,inlen,inbeg,zword)
ZIPINT inbuf,zword;
short inlen,inbeg;
{
  ZIPCHAR *strbeg;
  short len = 0;
  short zchrs_used = 0;
  short cs;
  char c;
  char sibreak = 0;
  char zchrs[CHRS_PER_ZWORD];			/* store bytes here */
  short i,j,twrd;
  strbeg = (ZIPCHAR *)(inbuf + dataspace + inbeg); /* get quick pointer */
  while(ZTRUE) {
    if (sibreak)
      break;
    if (!notbrkc(c = *(strbeg+len))) {
      if (len > 0) 				/* make sibreaks work */
	break;					/* stop when hit break */
       else
	sibreak = 1; }
    len++;
    if (zchrs_used < CHRS_PER_ZWORD) {		/* we still have room */
      cs = chrbyt(c);				/* 0 - 78 */
      if (cs > 26)  {				/* not default */
	zchrs[zchrs_used++] = 3 + (cs - 1) / 26; /* temporary shift */
	if (zchrs_used >= CHRS_PER_ZWORD) 
	  continue;				/* maybe filled it up? */
	}
       else if (cs == 0) {
	zchrs[zchrs_used++] = 5;
	if (zchrs_used >= CHRS_PER_ZWORD) continue; }
      if (cs) 
	zchrs[zchrs_used++] = ((cs - 1) % 26) + 6;
       else {				/* ascii value */
	zchrs[zchrs_used++] = 6;	/* cs 2 indicator of ascii */
	if (zchrs_used < CHRS_PER_ZWORD) {
	  zchrs[zchrs_used++] = c >> ZCHRLEN; /* save high bits */
	  if (zchrs_used < CHRS_PER_ZWORD)
	    zchrs[zchrs_used++] = c & ZCHRMSK;	/* and low bits */
	  }
	}				/* end of else */
      }					/* end of big if then */
    }					/* end of while (ztrue) ...*/
  /* we now have to fill out zchrs with padchrs */
  while (zchrs_used <= (CHRS_PER_ZWORD - 1))
    zchrs[zchrs_used++] = PADCHR;
  twrd = 0;
  for (i=0; i < CHRS_PER_ZWORD; i++) {
    /* i is the character we're currently dumping */
    twrd = (twrd << ZCHRLEN) | (zchrs[i] & ZCHRMSK);
    if ((i % 3) == 2) THEN {		/* last character in a word */
      if ((i + 1) == CHRS_PER_ZWORD)
	twrd |= BIT16;			/* end of string bit */
      if (zword == 0) 
	rdwstr[i / 3] = twrd;
       else
	PTVWRD(zword +(2 * (i / 3)), twrd);
      twrd = 0;
      }
    }					/* end of for loop */
  return(len);
}

notbrkc(c)
char c;
{  /* 	Returns true if character c is not a break character. 
   */
    char *ptr = rbrks;

    lastbrk = 0;		/* initialize pointer to last break char */
    if (c)  {
      while (*ptr) 			/* while not at end of list */
        if (*ptr == c)			/* break found */
	  break;
        else
	  ptr++;			/* no break, check next char */
      if (*ptr)  { 			/* if match, then c is a break */
        lastbrk = ptr;			/* save ptr to last break char */
        return(ZFALSE);
        }
      else
        return(ZTRUE); 
      }
    else
      return(ZFALSE);
}

char_cs(c)
char c;
{  /*	Given a character c, return its character set.
   */
   int i = 78;
   char *ptr;
   if (!c) return(3);
   ptr = &character_set[0];
   while (i--) {
     if (c == *ptr++) break; }
   if (i == -1) return(2);
   return((77 - i) / 26); }


char chrbyt(c)
char c;
{  /*	Given a character c, return its zchar value.
   */
    char *zptr;
    int i = 0;
    int won = 0;
    zptr = &character_set[0];
    while (*zptr) {
      if (c == *zptr++) {
	won = 1;
	break; }
      i++; }
    if (won)
      return(i + 1);
    return(0);
}

ZIPINT lookup(lexicon)
ZIPINT lexicon;
{  /*	Lookup searches the supplied vocabulary with the target
	being in rdwstr.  A zero is returned if the word is not found and
	otherwise the pointer into the vocab table is returned.
	(EZIP.  The matchings in the binary search must look at the third
	zword that has been created.)
	(lexicon has as its first byte the length of the sibreaks table)
        datwrd and datbyt are used to allow auxiliary vocabulary tables
        in pure space.  Hope it's not too slow.
   */
    unsigned short vocptr;
    short vwlen,vwords;
    short hi, lo, index, bottom, top;
    short third;
    lexicon = lexicon + datbyt(lexicon) + 1;	/* point past sibreaks */
    vwlen = datbyt(lexicon);			/* bytes per entry */
    vwords = datwrd(++lexicon);			/* number of words */
    lexicon += 2;				/* skip that too */
    if (vwords > 0) {				/* binary search */
      top = vwords;
      bottom = 1;
      do { 		/* B I N A R Y   S E A R C H  */
	index = (bottom + top) / 2;		/* point near the middle */
	vocptr = lexicon + ((index - 1) * vwlen); /* actually address a word */
	if ((hi = datwrd(vocptr)) == rdwstr[0])
	  if ((lo = datwrd(vocptr+2)) == rdwstr[1])
	    if ((third = datwrd(vocptr+4)) == rdwstr[2])
	       return(vocptr);
           else
	    if (third < rdwstr[2]) 
	      bottom = index + 1;		/* move up the table */
	     else
	      top = index - 1;		/* back up in table */ 
          else
	   if (lo < rdwstr[1])
	     bottom = index + 1;		/* move up the table */
	    else
	     top = index - 1;		/* back up in table */ 
        else
         if (hi < rdwstr[0]) 
	   bottom = index + 1;		/* move up the table */
          else
	   top = index - 1;		/* move down table */
       } while (bottom <= top);		/* loop until found or not */
      return(ZFALSE);			/* return that no word found */
      }
     else {				/* linear search */
      vwords = -vwords;
      while (vwords-- > 0) {
	if ((datwrd(lexicon) == rdwstr[0]) &&
	    (datwrd(lexicon+2) == rdwstr[1]) &&
	    (datwrd(lexicon+4) == rdwstr[2])
            ) 
           return(lexicon);
        lexicon += vwlen;
        }
      }
}

getlin(buffer,timeout,timerout)
short buffer;
ZIPINT timeout;
short timerout;
{  /*	Getlin takes a buffer that has first byte (read-only) containing
	the maximum number of input characters.  After the line is read
	in, it is lower casified.  The number of bytes read is returned.
   */
    short cnt, maxchars, cchars=0;
    int savx, savy, i;
    ZIPCHAR *ptr;
    ZIPCHAR *savptr;

    if (timerout == 0) 
      timeout = 0;			/* no timeout if no routine */
    maxchars = GTVBYT(buffer);		/* first byte has length */
    ptr = dataspace + buffer;	/* get pointer to entry point */
    ptr += 2;
    savptr = ptr;
    cchars = GTVBYT(buffer+1);	/* second byte has chars already there */
    for (cnt = 0; (cnt < cchars) && (cnt < maxchars); cnt++)
     inbuf[cnt] = *ptr++;
    ptr = savptr;
    while (ZTRUE) {
      cchars = md_getl(inbuf, maxchars, cchars, timeout);
      savx = scrx;
      savy = scry;	/* save cursor pos in case timeout */
      /* if cchars negative, we have - that many chars in buffer, and we timed
         out; if positive, we have that many, exclusive of the null */
      if (cchars < 0)  {		/* we timed out */
        cchars = -1 - cchars;		/* make sure cchars is correct */
	if (internal_call(timerout))	/* routine aborted the read */
	  return(-1);
	 else
          if (savx != scrx || savy != scry) { /* cursor position changed */
	    for (i = 0 ; i < cchars; i++)   /* reprint the input buffer */
               md_putc(*(inbuf+i));
            }
	  continue;			/* nope, try again */
	 }	/* end of if (cchars)  clause */
	else
	 break;	/* we didn't time out, so go deal with this */
	}
    for (cnt = 0; cnt < maxchars; cnt++)
      if ((inbuf[cnt] == 0) || (inbuf[cnt] == 26))
	break;
      else 
	*ptr++ = lc(inbuf[cnt]);	/* lowercasify input */
    *ptr = NULL;			/* end with zero */
    return(cnt);
}

flushwrds(strbeg, stroff, strlen)
short stroff, strlen;
ZIPCHAR *strbeg;
{  /* 	Flushwrds is called when the token table is filled and there is
	more input to be parsed.  The remainder of the ascii buffer is
	flushed and the characters thrown away are reported to the user.
   */
    md_printstr("Too many words typed, flushing: ");
    strbeg[strlen+1] = NULL;
/*    PTVBYT(rdeos, NULL); */	/* put a null in at eos */
    zmprnt(strbeg + stroff);	/* print flushed string */
    return;
}

char do_mouse_event(tab)
int *tab;
{
  ZIPINT extab;
  if (get_mouse_event(tab)) THEN {
    if ((extab = GTVWRD(PEXTAB)) && (GTVWRD(extab) > 1)) THEN {
      if (graphics > 0) THEN {
	PTVWRD(extab + (2 * PMSLOCX), tab[1] + 1);
	PTVWRD(extab + (2 * PMSLOCY), tab[2] + 1); }
       else {
	PTVWRD(extab + (2 * PMSLOCX), (tab[1] / 8) + 1);
	PTVWRD(extab + (2 * PMSLOCY), (tab[2] / 8) + 1); }
      return(253); } }
  return(0);
}

/* eof */

