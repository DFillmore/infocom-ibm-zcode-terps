/************************************************************************
*									*
*	R E A D   A N D   S U P P O R T I N G   R O U T I N E S 	*
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"
#include "ctype.h"	/* for islower() */

#define SPACE_LEFT (rdnwds < maxtoks)
#define WORDS_LEFT (curoff <= numread)

zread()
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
   */
    short numread, 		/* number of bytes read in */
	maxtoks;		/* number of tokens allowed in tbl 2 */
    ZIPINT rdret,		/* pointer to table 2 where tokens go */
	rdnwds,			/* token counter */
	wordent,		/* current entry ptr in table 2 */
	lookup();
    ZIPINT inbuf=argblk[1];
    ZIPINT lexv=argblk[2];
    short argct=argblk[0];
    short timeout=argblk[3];
    short timeroutine=argblk[4];

#ifdef ZIP
    statusln();				/* update status line */
#endif
    putchr(Z_EOL);			/* flush output buffer */
    dumpbuf();				/* without a crlf */
    linecnt = 0;			/* reset MORE line counter */
#ifdef EXZIP
    if (argct == 4) THEN		/* do call with timeout args */
        numread = getlin(inbuf,timeout,timeroutine);
      else				/* do call with no timeout */
       numread = getlin(inbuf,0,0);
    if (numread < 0) THEN
      return;				/* means getlin aborted */
#else
    numread = getlin(inbuf,0,0);	/* zip case doesn't time out */
#endif
    if (scripting) THEN
      fprintf(scrptfd, "%s\n", inbuf);
    rdbos = (ZIPCHAR *)(inbuf + dataspace + 1); /* table 1 pointer */ 
    rdeos = inbuf + numread + 1;	/* make a pointer to eostring */
    rdret = lexv + 1;		/* table 2 token count reposit */
    if (numread == 0) THEN {
      PTVBYT(rdret,0);
      return;
      } 
    rdnwds = 0;				/* no words parsed, ...yet */
    maxtoks = GTVBYT(lexv);		/* max tokens in input buffer */
    if (maxtoks > TOKEN_TBL_LEN) THEN	/* fix ZAP bug which allows too */
      maxtoks = TOKEN_TBL_LEN;
    wordent = lexv + 2; 		/* words begin after count bytes */
    curword = rdbos;
    while (notbrkc(*curword) == ZFALSE)
      curword++;			/* scan of leading spaces */
    curoff = curword - rdbos + 1;
 
    while (WORDS_LEFT && SPACE_LEFT) {
      getasc();				/* get ascii word */
      makezwrd();			/* make a zword of it */
      PTVWRD(wordent, lookup());	/* store 16 bit offset in table 2 */
      PTVBYT(wordent+2, POP());		/* fill in length value */
      PTVBYT(wordent+3, curoff);	/* fill in offset value */
      rdnwds++;				/* increment number of words read */
      wordent += 4;			/* skip to next word entry */
      curword = nxttok;			/* get pointer to next ascii word */
      curoff = curword - rdbos + 1;	/* update pointer */
      }
    if (WORDS_LEFT) THEN
      flushwrds(curoff);
    PTVBYT(rdret, rdnwds);
    return;
}

getasc()
{   /* 	Get the next ascii word, advance pointers, and return the length 
	and offset on the stack.
    */

    short len = 1;			/* initialize */
    ZIPCHAR *curbyt;
    ZIPCHAR *wordend;
    ZIPCHAR *nxtasc();

    curbyt = curword;			/* get current offset */
    if (notbrkc(*curbyt)) THEN
      while (notbrkc(*(curbyt+1))) {	/* if char is not a break */
        len++;				/* then bump our pointers */
        curbyt++;
        }
    wordend = curword + len;		/* pointer to end of ascii word */
    nxttok = nxtasc(wordend);		/* advance nxttok to next token */
    PUSH(len);				/* and length of ascii string */
    return;
}

ZIPCHAR *nxtasc(wordend)
ZIPCHAR *wordend;
{  /*	Nxtasc advances the pointer to the next letter or self-inserting
	break character; consequently the pointer is left pointing at the
	next thing to be tokenized.
   */
    if ((lastbrk) && (lastbrk < esibrks)) THEN
      lastbrk = esibrks;		/* force thru loop one for si brk */
    while ((lastbrk) && (lastbrk >= esibrks)) 	/* look for a break char */
      if (notbrkc(*wordend)) THEN 	/* look for next non break or si */
	break;
      else
	if (lastbrk >= esibrks) THEN
	  wordend++;			/* check next char */
    return(wordend);
}

notbrkc(c)
char c;
{  /* 	Returns true if character c is not a break character. 
   */
    char *ptr = rbrks;

    lastbrk = 0;		/* initialize pointer to last break char */
    if (c) THEN {
      while (*ptr) 			/* while not at end of list */
        if (*ptr == c) THEN		/* break found */
	  break;
        else
	  ptr++;			/* no break, check next char */
      if (*ptr) THEN { 			/* if match, then c is a break */
        lastbrk = ptr;			/* save ptr to last break char */
        return(ZFALSE);
        }
      else
        return(ZTRUE); 
      }
    else
      return(ZFALSE);
}

makezwrd()
{  /*	Makezwrd takes the maximum length of the ascii input word and
	passes it to zword to convert the word into a zstring.
	(EZIP.  Storage space in zascii should have used an equate.
	I apologize.  It should be expanded to 9 for EZIP.)
   */
    short i, len;
    char zascii[7];			/* ascii storage */

    len = POP();			/* get length from stack */
    PUSH(len);				/* put value back on stack */
    for (i = 0; i <= CHRS_PER_ZWORD; i++) 
      if (i >= len) THEN
	zascii[i] = 0;
      else
	zascii[i] = *(curword + i);
    zword(zascii);
    return;
}
 
zword(ptr)	
char *ptr;
{  /*	Make a zword out of the ascii word pointed to by ptr and return 
    	results in global rdwstr.
   */
    short cs, i, j;
    ZIPBYT zbyte, chrbyt();
    char zchrs[CHRS_PER_ZWORD];		/* repository during conversion */

    for (i = 0; i < CHRS_PER_ZWORD; i++) {	/* for each char */
      if (*ptr) THEN {			/* if it is a char */
	cs = char_cs(*ptr);		/* figure character set */
	if (cs) THEN {			/* if cs other than 0 */
	  cs += 3;			/* calculate a temporary shift */
	  zchrs[i] = cs;		/* save it */
 	  if (++i == CHRS_PER_ZWORD) THEN
	    break;
	  }
	zbyte = chrbyt(*ptr++);		/* get zchar byte value */	
        if (zbyte) THEN 		/* found, so save char */ 
	  zchrs[i] = zbyte; 
	else {				/* char not found, use ASCII */
	  zchrs[i] = 6;			/* cs 2 indicator of ASCII */
	  if (++i != CHRS_PER_ZWORD) THEN {/* if not last char then save */
	    zchrs[i] = *(ptr-1) >> ZCHRLEN;	/* save hi bits */
	    if (++i != CHRS_PER_ZWORD) THEN
	      zchrs[i] = *(ptr-1) & ZCHRMSK;	/* save low bits */
	    else
	      break;
	    }
	  else
	    break;
	  }
	}				/* end if (*ptr)  */
      else
	zchrs[i] = PADCHR;		/* save a pad character */
      }					/* end of for loop */
    for (i = 0; i < (CHRS_PER_ZWORD / 3); i++)
      rdwstr[i] = 0;			/* initialize to zero */
    j = -1;
    for (i = 0; i < CHRS_PER_ZWORD; i++) {	/* fill string */
      if ((i % 3) == 0) THEN		/* change words every three bytes */
	j++; 
      rdwstr[j] = (rdwstr[j] << ZCHRLEN) | (zchrs[i] & ZCHRMSK);
      }
    rdwstr[CHRS_PER_ZWORD / 3 - 1] |= BIT16;	/* turn on end of string bit */
    return;
}

char_cs(c)
char c;
{  /*	Given a character c, return its character set.
   */
    if (c) THEN				/* if char is not null */
      if (islower(c)) THEN
	return(0);			/* it is either lower or...*/
      else
	return(2);			/* 3rd or ascii */
    else
      return(3);			/* nope, null */
}

ZIPBYT chrbyt(c)
char c;
{  /*	Given a character c, return its zchar value.
   */
    char *zptr;

    switch (char_cs(c)) {
      case 0: return(c - 'a' + 6);	/* first char is 6 */
      case 2: {				/* never upper case! */
	zptr = zchars + 51;
	while (*++zptr) {
	  if (*zptr == c) THEN
	    return((zptr - zchars - 52) + 6);
	  }
	return(0);			/* return failure */
	}
      default: return(0);
      }
}

ZIPINT lookup()
{  /*	Lookup performs a binary search on the vocabulary with the target
	being in rdwstr.  A zero is returned if the word is not found and
	otherwise the offset into the vocab table is returned.
	(EZIP.  The matchings in the binary search must look at the third
	zword that has been created.)
   */
    unsigned short vocptr;
    short hi, lo, index, bottom, top;
#ifdef EXZIP
    short third;
#endif
    top = vwords;
    bottom = 1;
   
   do { 		/* B I N A R Y   S E A R C H  */

    index = (bottom + top) / 2;		/* point near the middle */
    vocptr = vocbeg + ((index - 1) * vwlen);	/* actually address a word */
    if ((hi = GTVWRD(vocptr)) == rdwstr[0]) THEN
      if ((lo = GTVWRD(vocptr+2)) == rdwstr[1]) THEN
#ifdef EXZIP
	if ((third = GTVWRD(vocptr+4)) == rdwstr[2]) THEN
#endif
	  return(vocptr);
#ifdef EXZIP
        else
	  if (third < rdwstr[2]) THEN
	    bottom = index + 1;		/* move up the table */
	  else
	    top = index - 1;		/* back up in table */ 
#endif
      else
	if (lo < rdwstr[1]) THEN
	  bottom = index + 1;		/* move up the table */
	else
	  top = index - 1;		/* back up in table */ 
    else
      if (hi < rdwstr[0]) THEN
	bottom = index + 1;		/* move up the table */
      else
	top = index - 1;		/* move down table */
    }
    while (bottom <= top);		/* loop until found or not */
    return(ZFALSE);			/* return that no word found */ 
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
    ZIPCHAR *ptr;

    if (timerout == 0) THEN
      timeout = 0;			/* no timeout if no routine */
    maxchars = GTVBYT(buffer);		/* first byte has length */
    ptr = dataspace + buffer + 1;	/* get pointer to entry point */
    while (ZTRUE) {
      cchars = md_getl(inbuf, maxchars, cchars, timeout);
      /* if cchars negative, we have - that many chars in buffer, and we timed
         out; if positive, we have that many, exclusive of the null */
      if (cchars < 0) THEN {		/* we timed out */
        cchars = -1 - cchars;		/* make sure cchars is correct */
	if (internal_call(timerout)) THEN /* routine aborted the read */
	  return(-1);
	 else
	  continue;			/* nope, try again */
	 }	/* end of if (cchars) THEN clause */
	else
	 break;	/* we didn't time out, so go deal with this */
	}
    for (cnt = 0; cnt < maxchars; cnt++)
      if ((inbuf[cnt] == 0) || (inbuf[cnt] == 26)) THEN
	break;
      else 
	*ptr++ = lc(inbuf[cnt]);	/* lowercasify input */
    *ptr = NULL;			/* end with zero */
    return(cnt);
}

flushwrds(stroff)
short stroff;
{  /* 	Flushwrds is called when the token table is filled and there is
	more input to be parsed.  The remainder of the ascii buffer is
	flushed and the characters thrown away are reported to the user.
   */
    printf("Too many words typed, flushing: ");
    PTVBYT(rdeos, NULL);	/* put a null in at eos */
    zmprnt(rdbos + stroff);	/* print flushed string */
    return;
}
