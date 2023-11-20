/************************************************************************
*									*
*	I N S T R U C T I O N   D I S P A T C H				*
*									*
************************************************************************/

#include "zipdefs.h"
#include "extern.h"
#include <setjmp.h>

ZIPCHAR *flagptr();
ZIPCHAR *objloc();

/* dispatch for two ops */
void run_two_op(operand)
ZIPINT operand;
{
  short ts1,getvar();
  ZIPCHAR *ptr;
  ZIPCHAR *objptr2;
  ZIPCHAR *objptr;
  ZIPBYT t1,t2,id;
  ZIPOBJ objx;
  ZIPINT temp,nxtprp(),firstprp();
#if _DEBUG
  if (debug) THEN
    dinfo(2, operand);		/* opcode information */
#endif
  switch (operand) {		/* find two op */
    case OPQEQU : {		/* EQUAL? */
	PRED(argblk[1] == argblk[2]);
	return;
	}
    case OPQLES : {		/* LESS? */
	PRED(argblk[1] < argblk[2]);
	return;
	}
    case OPQGRT : {		/* GREATER? */
	PRED(argblk[1] > argblk[2]);
	return;
	}
    case OPQDLE : {		/* DECREMENT LESS? */
	ts1 = getvar(argblk[1]);	/* get variable */
	putvar(argblk[1],--ts1);	/* set dec'ed var */
	PRED(ts1 < argblk[2]);
	return;
	}
    case OPQIGR : {		/* INCREMENT GREATER? */
	ts1 = getvar(argblk[1]);
	putvar(argblk[1],++ts1);
	PRED(ts1 > argblk[2]);
	return;
	}
    case OPQIN : {		/* IN? */
	objptr = objloc(argblk[1]);
	PRED(GETOID(objptr+PARENT) == OBJARG(2));
	return;
	}
    case OPBTST : {		
	PRED((~argblk[1] & argblk[2]) == 0);
	return;
	}
    case OPBOR : {		
	putval(argblk[1] | argblk[2]);
	scrchk = 1;		/* instruction used to toggle script */
	return;
	}
    case OPBAND : {		
	putval(argblk[1] & argblk[2]);
	scrchk = 1;		/* check scripting bit */
	return;
	}
    case OPQFSE : {		/* FlagSET? */
	ptr = flagptr(argblk[1],&argblk[2]);
	t1 = BIT8;
	PRED(GTABYT(ptr) & (t1 >> argblk[2]));
	return;
	}
    case OPFSET : {		/* FlagSET */
    	ptr = flagptr(argblk[1],&argblk[2]);
	t1 = BIT8;
	t2 = GTABYT(ptr) | (t1 >> argblk[2]);
	PTABYT(ptr, t2);
	return;
	}
    case OPFCLE : {		/* FlagCLEAR */
    	ptr = flagptr(argblk[1],&argblk[2]);
	t1 = BIT8;
	t2 = GTABYT(ptr) & (~(t1 >> argblk[2]));
	PTABYT(ptr, t2);
	return;
	}
    case OPSET : {		
	putvar(argblk[1],argblk[2]); 
	return;
	}
    case OPMOVE : {			/* (EZIP - remove BYTARGS) */
	zremove(argblk[1]);		/* remove object from cont */
	objptr = objloc(argblk[1]);	/* find loc of obj2 */
	objptr2 = objloc(argblk[2]);	/* and that of obj1 */
	PUTOID(objptr+PARENT, OBJARG(2)); /* obj2 into obj1's loc*/
	objx = GETOID(objptr2+CHILD);	/* get contents of 2's 1st */
	PUTOID(objptr2+CHILD, OBJARG(1)); /* obj1 now 1st in obj2*/
	if (objx != EMPTY) THEN 	/* chain into sibling */
	  PUTOID(objptr+SIBLING, objx);/* yes, chain into 1's sib */
	return;
	}
    case OPGET : {
	argblk[2] <<= 1;
	argblk[1] += argblk[2]; 	/* make an index into table*/
	putval(datwrd(argblk[1]));		/* return the word */
	return;
	}
    case OPGETB : {
	argblk[1] += argblk[2];		/* make an index into table*/
	bytval(datbyt(argblk[1]));		/* return the byte */
	return;
	}
    case OPGETP : {	/* (EZIP remove BYTARGS-no-6 bit id) */
	temp = firstprp(argblk[1]);	/* get property pointer */
	do {
	    id = GTVBYT(temp);		/* get property id */
	    id &= PNUMSK;			/* isolate id bits */
	    if (id < BYTARG(2)) THEN {
	      temp = ((--argblk[2]) << 1) + objtab; /* use default */
	      break;
	      }
	    if (id == BYTARG(2)) THEN {	/* got it */
	       id = (GTVBYT(temp++)) & PSZMSK;	/* now find length */
	       if (id == 0)	THEN {	/* length bits 0 => byte */
		 bytval(GTVBYT(temp));
		 return;
		 }
		else break;	/* word value */
		}				/* end of if (id==BYT) */
	    temp = nxtprp(temp);		/* get next property */
	    }
	 while (ZTRUE);			/* loop until prop found */
	putval(GTVWRD(temp));		/* return the word */
	return;
	}
    case OPGTPT : {			/* GET PROPERTY TABLE */
	temp = firstprp(argblk[1]);	/* get property pointer */
	do {
	    id = GTVBYT(temp);		/* get property id */
	    id &= PNUMSK;			/* isolate id bits */
	    if (id == BYTARG(2)) THEN {	/* got it */
	    if (GTVBYT(temp) & 0x80) temp++;	/* length byte? */
	    temp++;
	    break;
	    }
	if (id < BYTARG(2)) THEN {	/* no such prop */
	   temp = 0;
	   break;
	   }
	temp = nxtprp(temp);		/* get next property */
	}
	 while (ZTRUE);			/* loop until prop found */
	putval(temp);		/* and return the property pointer */
	return;
	}
    case OPNEXT : {
	temp = firstprp(argblk[1]);	/* get property pointer */
	if (argblk[2]) THEN {		/* if not a zero prop */
	  do {				/* find next property */
	      id = GTVBYT(temp);		/* get prop id */
	      id &= PNUMSK;		/* mask out size bits */
	      if (id == BYTARG(2)) THEN break;
	      if (id < BYTARG(2)) THEN
		fatal("Property not found");
	      temp = nxtprp(temp);	/* get next property */
	      }
	    while (ZTRUE);		/* do until found */
	  temp = nxtprp(temp);		/* get next prop */
	  }				/* end of if non-zero prop */
	putval((GTVBYT(temp) & PNUMSK));/* return the property */
	return;
	}
    case OPADD : {
	putval(argblk[1] + argblk[2]);
	return;
	}
    case OPSUB : {
	putval(argblk[1] - argblk[2]);
	return;
	}
    case OPMUL : {
	putval(argblk[1] * argblk[2]);
	return;
	}
    case OPDIV : {
	putval(argblk[1] / argblk[2]);
	return;
	}
    case OPMOD : {
	putval(argblk[1] % argblk[2]);
	return;
	}
    case OPCALL2: {
	argblk[0] = 2;
	docall(ZTRUE);
	return;
	}
    case OPICALL2: {
	argblk[0] = 2;
	docall(ZFALSE);			/* says don't return a value */
	return; }
    case OPCOLOR: {
	flush_buffer();
	if (docolor <= 0) THEN return;
	md_set_color(argblk[1],argblk[2]);
	return; }
    case OPTHROW: {
	do_throw(argblk[1],argblk[2]);
	return; }
    default : fatal("Undefined 2-op");	
  }				/* end of switch statement */
}

void run_zero_op(opcode)
ZIPINT opcode;
{
  opcode = (opcode & ZEROMSK) + ZERO_OP;
}

void get_ext_args(opcode)
ZIPINT opcode;
{
  short getarg();
  int jx = 0, ix, temp;
  ZIPBYT adrmode2, adrmode;
  adrmode = nxtbyt();
  if ((opcode == OPXCALL) || (opcode == OPIXCALL)) {
    adrmode2 = nxtbyt();		/* second mode byte */
    for (ix = 1; ix <= 4; ix++) {
      argblk[++jx] = getarg((adrmode >> 6) & 3);
      adrmode <<= 2; }
    adrmode = adrmode2; }
  for (ix = 1; ix <= 4; ix++) {
    if ((temp = ((adrmode >> 6) & 3)) == 3) THEN
      break;
    argblk[++jx] = getarg(temp);
    adrmode <<= 2; }
  argblk[0] = jx;
}

void run_ext_op(opcode)
ZIPINT opcode;
{
  ZIPBYT ix,id;
  ZIPINT operand;
  ZIPINT temp;
  int modulator;
  char adrmode,adrmode2,number[7];
  if (opcode < 256) THEN
    opcode = (opcode & EXTMSK) + EXT_OP;		/* actual opcode */
  get_ext_args(opcode);
#if _DEBUG
  if (debug) THEN 
    dinfo(3, opcode);		/* display opcode information */
#endif
  switch (opcode) {
    case XQEQU : {			/* EXTENDED EQUAL? */
	for (ix = 2; ix <= argblk[0]; ix++) {
	    if (argblk[1] == argblk[ix]) THEN {
	      ppred(ZTRUE);		/* a match!!!! */
	      return;
	      }
	     }
	ppred(ZFALSE);			/* no match found */
	return;
	}
    case OPCALL:
	docall(ZTRUE);
	return;
    case OPICALL:
	docall(ZFALSE);
	return;
    case OPPUT : {
	argblk[2] <<= 1;		/* convert words to bytes */
	argblk[1] += argblk[2];		/* add in offset */
	PTVWRD(argblk[1],argblk[3]);	/* put word arg3 @offset */
/* ifdef ZIP */
	if (scrchk) THEN		/* (EZIP remove these) */
	  chkscript();
/* #endif */
	return;
	}
    case OPPUTB : {
	argblk[1] += argblk[2];		/* figure index */
	PTVBYT(argblk[1],BYTARG(3));	/* return byte */
	return;
	}
    case OPPUTP : {
	temp = firstprp(argblk[1]);	/* get property pointer */
	do {
	  id = GTVBYT(temp);		/* get property id */
	  id &= PNUMSK;			/* isolate id bits */
	  if (id < BYTARG(2)) THEN 
	    fatal("Property not found");	/* bye-bye ... */
	  if (id == BYTARG(2)) THEN {		/* got it */
	    id = (GTVBYT(temp++)) & PSZMSK;	/* now find length */
	    if ((id >>= PROPSIZE) == 0) THEN {	/* right justify */
	      PTVBYT(temp,BYTARG(3));		/* and store byte */
	      return;
	      }
	     else break;
	     }
	   temp = nxtprp(temp);		/* get next property */
	   }
	    while (ZTRUE);			/* loop until prop found */
	PTVWRD(temp, argblk[3]);	/* and return the prop */
	return;
	}
    case OPREAD : {
	putval(zread());		/* xzip read returns a value */
	return;
	}
    case OPLEX: {				/* interface to LEX */
	if (argblk[0] < 4) THEN {		/* default the arguments */
	  argblk[4] = 0;			/* preserve argument */
	  if (argblk[0] < 3) THEN
	    argblk[3] = 0;			/* lexicon argument */
	  }
	zlex(argblk[1],argblk[2],argblk[3],argblk[4]);
	return; }
    case OPZWSTR: {
	zwstr(argblk[1],argblk[2],argblk[3],argblk[4]);
	return; }
    case OPPRNC : {			/* PRINT CHAR */
	putchr(argblk[1]);
	return;
	}
    case OPPRNN : {			/* PRINT NUMBER */
        print_number(argblk[1],putchr);
	return;
	}
    case OPRAND : {			/* RANDOM (EZIP) */
	dorand();
	return;
	}
    case OPPUSH : {
	PUSHZ(argblk[1]); 
	return;
	}
    case OPPOP : {
	putvar(argblk[1], POPZ());
	return;
	}
    case OPSPLT : {			/* SPLIT (EZIP) */
	dosplit();
	return; }
    case OPSCRN : {			/* SCREEN (EZIP) */
	doscreen();
	return; }
/* EZIP instructions */
    case OPXCALL: {
	docall(ZTRUE);
	return;
        }
    case OPIXCALL: {
	docall(ZFALSE);
	return; }
    case OPCLEAR: {
	flush_buffer();
	current_highlight = 0;
	if (argblk[1] == -1) {
	   spltflg = 0;	/* unsplit */
	   winlen = slpp;	/* make sure window length OK */
	   scr1x = 0;
	   scr1y = 0;
	   if (screen != 0) {
	     scripting = scrhld;
	     scrhld = 0; }
	   screen = 0;
	   if (mouseflg)
	     md_mouse(HIDE_MOUSE);
	   md_clr(0);	/* clear whole screen */
	   }
	  else
	   md_clr(argblk[1]);
	return;
	}
    case OPERASE: {
	flush_buffer();
	if (argblk[1] == 1)
	  md_clear_eol();		/* clear to EOL */
	  return;
	  }
    case OPCURSET: {
	flush_buffer();
	if (screen == 1) THEN
	  locate((argblk[1]-1) / fontsize, (argblk[2]-1) / fontsize);
	return;
	}
    case OPHLIGHT:
	md_hlite(argblk[1]);
	return;
    case OPBUFOUT:
	if (rdir) THEN			/* if dumping to table */
	  prevbuf = argblk[1];		/* don't try to buffer it */
	 else
	  if ((bufflg = argblk[1]) == 0) {	/* 0->off,else on */
	    flush_buffer();
	    }
	return;
    case OPDIROUT: {
	dodirout();
	return; }
    case OPINPUT:
	if (argblk[1] == 1) {	/* only kb defined */
	  if (bufflg) {
	    flush_buffer();	/* flush buffered output */
	    }
	  if (argblk[0] == 3)	/* time out version? */
	    bytval(md_tinp(argblk[2],argblk[3]));
	   else bytval(md_inp());		/* wait for chr */
	  }
	 else {
	  bytval(md_inp());
	  }
	return;
    case OPSOUND: {      /* 1 - beep, 2 - boop */
      switch(argblk[1]) {
        case 1:  
	  md_sound(BEEP);
	  break;
	case 2:
	  md_sound(BOOP);
	  break;
	default:
	  fatal("undefined sound");
	}
	return;}

/* unimplemented */
    case OPDIRIN:	/* input from kb (0) or script file */
    case OPCURGET:	/* get cursor */
	flush_buffer();
        PTVWRD(argblk[1],(scry * fontsize) + 1);
	PTVWRD(argblk[1]+2,(scrx * fontsize) + 1);
	return;
    case OPMARGIN: {
	domargin();
	return; }
    case OPINTBL:
	if (argblk[0] > 3) THEN
	  do_intbl(argblk[1],argblk[2],argblk[3],argblk[4]);
	 else
	  do_intbl(argblk[1],argblk[2],argblk[3],0202);
	return;

    case OPASHIFT:				/* arithmetic shift */
	if (argblk[2] < 0) THEN
	  putval(argblk[1] >> -argblk[2]);	/* always arithmetic shift */
	 else					/* since argblk is signed */
	  putval(argblk[1] << argblk[2]);
	return;
    case OPSHIFT:				/* logical shift */
	if (argblk[2] > 0) THEN
	  putval(argblk[1] << argblk[2]);	/* right shift is OK */
	 else
	  putval (((unsigned) argblk[1]) >> -argblk[2]);
	  return;				/* force logical shift */
    case OPASSNQ:				/* assigned? */
	PRED(argblk[1] <= zargct);		/* this guy was an argument */
	return;
    case OPBCOM:				/* complement */
	putval(~argblk[1]);
	return;
    case OPCOPYT:
	do_table_copy(argblk[1],argblk[2],argblk[3]);
	return;
    case OPPRINTT:
	if (argblk[0] < 4) THEN {		/* default optionals */
	  argblk[4] = 0;
	  if (argblk[0] < 3) THEN
	    argblk[3] = 1;
	  }
	do_table_print(argblk[1],argblk[2],argblk[3],argblk[4]);
	return;
    case OPXSAVE:
	if (argblk[0] == 0) THEN
	  putval(sav_res(OPSAVE));
	 else
	  putval(partial_sav_res(OPSAVE,argblk[1],argblk[2],argblk[3]));
	return;
    case OPXRESTORE:
	if (argblk[0] == 0) THEN
	  putval(sav_res(OPREST));
	 else
	  putval(partial_sav_res(OPREST,argblk[1],argblk[2],argblk[3]));
	return;
    case OPISAVE:
	if (!isave_allowed) THEN
	  putval(-1);			/* user disabled it, or no memory */
	 else
	  putval(internal_sav_res(OPSAVE));
	return;
    case OPIRESTORE:
	if (!isave_happened) THEN
	  putval(0);			/* save never done successfully */
	 else
	  putval(internal_sav_res(OPREST));
	return;
    case OPMSEINFO:
	mouse_info(argblk[1]);
	return;
    case OPXPUSH:
	xpush(argblk[1],argblk[2]);
	return;
    case OPFSTACK:
	fstack(argblk[1],argblk[2]);
	return;
    case OPFONT:
	flush_buffer();
	putval(md_set_font(argblk[1]));	/* sets font for this window, maybe */
	return;
    case OPPICINF:
	ppred(ZFALSE);
	return;
    case OPDISPLAY: case OPDCLEAR:
	return;				/* not implemented */
    default : {
	if ((operand = opcode - EXT_OP) <= LAST_TWO_OP) THEN
	  run_two_op(operand);
	 else		
	  fatal("Undefined Ext-Op");
	}				/* end of default */
  }					/* end of switch statement */
}

/* do MARGIN */
domargin()
{
  if (screen != 0) THEN
    return;
  argblk[1] = argblk[1] / fontsize;
  argblk[2] = argblk[2] / fontsize;
  if ((argblk[1] >= scrwid) || (argblk[2] >= scrwid)) THEN
    return;
  if (chars_on_line > 0) THEN
    return;
  LM = argblk[1];
  RM = argblk[2];
  PTVWRD(PLMRG, LM * fontsize);
  PTVWRD(PRMRG, RM * fontsize);
  curscrwid = scrwid - LM - RM;
  locate(scry, LM);
}

/* Do OPRAND */
dorand()
{
  int temp;
  if (argblk[1] <= 0) {			/* enable/disable */
    rcycle = -argblk[1];		/* save abs value */
    putval(rconst = rcycle); }
   else if (rcycle != 0) {		/* already disabled */
	  if (++rconst > rcycle) rconst = 1;
	  putval(rconst); }
   else {				/* normal */
    argblk[1] &= BYTEMSK;		/* argument <= 255 */
    temp = rand();
    putval((temp % argblk[1]) + 1);
    }
}

/* OPDIROUT */

dodirout()
{
  ZIPINT temp;
  flush_buffer();
  switch(argblk[1] & 0xFF) {
    case 1:		/* screen */
	vidflg = 1;
	break;
    case 2:		/* script */
	if (!scripting && !scrhld) {
	  temp = GTVWRD(PFLAGS);
	  PTVWRD(PFLAGS, temp | SCRIPTBIT);
	  chkscript(); }
	break;
    case 3:		/* table */
	prevbuf = bufflg;		/* remember buffering */
	bufflg = 0;			/* but don't do it */
	rdir = 1;			/* output is to table */
	rtable = argblk[2];		/* namely this one */
	rtable2 = rtable + 2;
	rdirout = 0;
	break;
    case 4:		/* file not supported */
	break;
    case 0xFF:		/* screen off */
	flush_buffer();
	vidflg = 0;
	break;
    case 0xFE:		/* script off */
	temp = GTVWRD(PFLAGS);
	PTVWRD(PFLAGS, temp & (~SCRIPTBIT));
	chkscript();
	break;
    case 0xFD:		/* table off */
	bufflg = prevbuf;
	rdir = 0;
	PTVWRD(rtable, rdirout);
	break;
    case 0xFC:
	break; }
}

/* OPSPLIT */
dosplit()
{
  ZIPINT temp;
  flush_buffer();
  if (temp = argblk[1]) THEN {	/* size of window 1 */
    if (temp >= slpp)
      temp = slpp - 1;/* max, inc scroll line */
     spltflg = temp;
     winlen = slpp - spltflg;
     if (spltflg > scry) THEN	/* we have to move screen 0 cursor */
       locate(spltflg,LM);		/* to top left */
     }
    else {
     winlen = slpp;
     if (screen == 1) THEN {
       scripting = scrhld;
       scrhld = 0;
       screen = 0;			/* so we don't move the cursor */
       scr0x = scr1x;
       scr0y = scr1y;
       }
      spltflg = topscr;
      } 
     if (mouseflg && spltflg) {	/* reveal the mouse only in scr 1 */
       md_mouse(SHOW_MOUSE);
       md_mouse(SET_MOUSE_Y_BOUNDS,0,0,spltflg); }
}

/* OPSCREEN */
doscreen()
{
  int newfont;
  flush_buffer();
  if (spltflg && (argblk[1] != screen)) {	/* split & new screen */
    if (argblk[1] == 1) {			/* screen 1 */
      scrhld = scripting;			/* suspend scripting */
      scripting = 0;
      if (mouseflg) THEN
	md_mouse(HIDE_MOUSE);
      scr0font = scrfont;
      newfont = scr1font;
      scr0x = scrx;
      scr0y = scry;
      scrx = scr1x;
      scry = scr1y; }
     else {
      scripting = scrhld;
      scrhld = 0;
      if (mouseflg) THEN
	md_mouse(SHOW_MOUSE);
      scr1font = scrfont;
      newfont = scr0font;
      scr1x = scrx;
      scr1y = scry;
      scrx = scr0x;
      scry = scr0y; }
    screen = argblk[1];
    md_set_font(newfont);
    locate(scry,scrx); }
}

void main_loop()
{
 /*	MAIN_LOOP is the heart of ZIP.  It picks up the next instruction
	using NXTBYT, decodes it, gathers it arguments, and performs
	the selected operation.  The loop consists of an initial opcode
	fetch and four case statements.  Each of the four types of opcodes
	have their own argument decoding schemes (as documented in the
	ZIP DOC).  There is one label used, EXTENT (for "EXTended op ENTry).
	2-ops can also be encoded as extended ops; consequently, the 2-op
	case statement must be entered a second time for this case.

	Note that if debugging is on, a call to the debugger is made before
	each instruction is executed (thus the ZPC will reflect that number
	of arguments picked up as well as the size of the opcode.)
	(EZIP add in new opcodes)
   */

   /*	Efficiency note: should avoid unnecessary initializing of automatic 
	vars. 
   */

/* split up into different cases to give optimizer a chance... */

    ZIPBYT operand,id;
    ZIPINT opcode;
    ZIPCHAR *objptr;
    ZIPOBJ objx;
    char adrmode;
    short getarg();
  while (1) {    			/* we only ever longjmp out of here */
#if _DEBUG
	if (debug) THEN debugger();
	else {
#endif
    opcode = nxtbyt();			/* get next opcode byte */
    if (opcode == OPEXTOP) THEN
      opcode = 256 + nxtbyt();		/* handle two-byte opcodes */
    if (opcode) THEN {			/* legal operation */
      if (opcode < ONE_OP) THEN {	/* it's a two op */
	if (operand = (opcode & TWOMSK)) THEN {	/* isolate operand */
	  adrmode = 1;			/* addressing mode immediate */
	  if (opcode & TWOMOD1) THEN	/* check for variable arg */
	    adrmode++;
	  argblk[1] = getarg(adrmode);	/* get argument by adrmode */
	  adrmode = 1;			/* reset to immediate */
	  if (opcode & TWOMOD2) THEN	/* check for variable arg */
	    adrmode++;
	  argblk[2] = getarg(adrmode);  /* get second arg by adrmode */
	  run_two_op(operand);
	  }				/* end of if (operand = opcode */
	else 				/* else it's not a valid TWO */
	  fatal("Undefined 2-op");		/* end of if(operand...) */
	}				/* end of if op > ONE_OP */
      else
	if (opcode < ZERO_OP) THEN {
	  operand = (opcode & ONEMODE) >> 4;	/* mode bits */
	  opcode = (opcode & ONEMSK) + ONE_OP;	/* operator bits */
	  argblk[1] = getarg(operand);		/* get the arg */
#if _DEBUG
	  if (debug) THEN
	    dinfo(1,opcode);
#endif
	  switch (opcode) {
		case OPQZER : {		/* ZERO? */
			PRED(argblk[1] == 0);
			break;
			}
		case OPQNEX : {		/* NEXT? */
			objptr = objloc(argblk[1]);	/* get location */
			objx = GETOID(objptr + SIBLING);/* get sib slot */
			OBJVAL(objx);		/* return the byte value */
			PRED(objx);			/* predicate return */
			break;
			}
		case OPQFIR : {		/* FIRST? */
			objptr = objloc(argblk[1]); /* get obj's location */
			objx = GETOID(objptr + CHILD);	/* get child slot */
			OBJVAL(objx);		/* return the byte value */
			PRED(objx);			/* predicate return */
			break;
			}
		case OPLOC : {
			objptr = objloc(argblk[1]); /* get obj's location */
			objx = GETOID(objptr + PARENT);	/* get obj's parent */
			OBJVAL(objx);			/* return byte value */
			break;
			}
		case OPPTSI : {			/* PROPERTY TBL SIZE */
			id = GTVBYT(argblk[1]-1);	/* get property id */
			id &= PSZMSK;			/* isolate size bits */
			id >>= PROPSIZE;	/* right justify bits */
			if (id >= 2)			/* variable length? */
			  id = GTVBYT(argblk[1]-1) & PNUMSK;
					/* length bits */
			 else id++;		/* 0 => 1 byte, 1 => 2 bytes */
			bytval(id);
			break;
			}
		case OPINC : {
			putvar(argblk[1], (getvar(argblk[1]) + 1));
			break;
			}
		case OPDEC : {
			putvar(argblk[1], (getvar(argblk[1]) - 1));
			break;
			}
		case OPPRNB : {			/* PRINT BYTE ALIGNED STRING */
			bspltb(argblk[1]);	/* get string pointer */
			putstr();
			break;
			}
    		case OPCALL1: {
			argblk[0] = 1;
			docall(ZTRUE);
			break;
			}
		case OPICALL1: {
			argblk[0] = 1;
			docall(ZFALSE);
			break; }
		case OPREMO : {			/* REMOVE */
			zremove(argblk[1]);
			break;
			}
		case OPPRND : {			/* PRINT OBJ'S DESCRIPTION */
			printd(argblk[1]);		/* print short desc */
			break;				/* and go away */
			}
		case OPRETU : {			/* RETURN */
			zret(argblk[1]);
			break;
			}
		case OPJUMP : {
			zpc2 += argblk[1] - 2;	/* offset - normalize */
			newzpc();
			break;
			}
		case OPPRIN : {			/* PRINT WORD ALIGNED STRING */
			bsplitq(argblk[1]); /* QUAD ALINGN IN EZIP and XZIP */
			putstr();
			break;
			}
		case OPVALU : {			/* VALUE */
			putval(getvar(argblk[1]));
			break;
			}
		default : fatal("Undefined 1-op");
  		}				/* end of switch statement */
	 }				/* end of if ONE_OP ... */
	else
	 if (opcode < EXT_OP) THEN {
	  opcode = (opcode & ZEROMSK) + ZERO_OP;
#if _DEBUG
	 if (debug) THEN
	  dinfo(0,opcode);
#endif
	 switch (opcode) {
		case OPRTRU : {		/* RTRUE */
			zret(ZTRUE);
			break;
			}
		case OPRFAL : {		/* RFALSE */
			zret(ZFALSE);
			break;
			}
		case OPPRNI : {		/* PRINT IMMEDIATE STRING */
			printi();
			break;
			}
		case OPPRNR : {		/* PRINT WITH CRLF */
			printi();		/* print immediate string */
			newlin();		/* with a carriage return */
			zret(ZTRUE);		/* and return a true */
			break;
			}
		case OPNOOP : break;
    		case OPSAVE : {		/* (EZIP make into VAL's) */
			if(sav_res(OPSAVE)) putval(1);	/* save ok */
			 else putval(0);		/* save failed */
			break;
			}
		case OPREST : {
			if(sav_res(OPREST)) putval(2);	/* restore ok */
			 else putval(0);		/* restore failed */
			break;
			}
		case OPRSTT : {			/* RESTART */
			newlin();		/* cr and flush buffer */
			restart(ZTRUE);			/* midgame restart */
			break;
			}
		case OPRSTA : {			/* RETURN VALUE ON STACK */
			zret(POPZ());
			break;
			}
		case OPCATCH: {			/* return a "frame pointer" */
			putval(zlocs);		/* this will get back to TOS */
			break; }
		case OPQUIT : {
			z_exit();		/* hasta lumbago, guys */
			break;
			}
		case OPCRLF : {
			newlin();
			break;
			}
		case OPUSL : {
			break;		/* STATUS LINE */
			}
		case OPVERI : {
			PRED(verify());
			break;
			}
		default : fatal("Undefined 0-op");
  		}				/* end of switch statement */
	  }				/* end of if ZERO_OP ... */
	else {
	  run_ext_op(opcode);
	  }					/* end of else not ZERO */
	}					/* end of if (opcode) ... */
    else {
#if _DEBUG
      if (debug) THEN
	dinfo(-1, opcode);
#endif
      fatal("Undefined operation");
      }
#if _DEBUG
    }					/* end of if (debug) */
#endif
  }		/* end of while... */
}						/* end of main_loop */

/************************************************************************
*									*
*	S H A R E D   O P C O D E S 					*
*									*
************************************************************************/

zremove(obj) 		/* OPREMO separated for use by OPMOVE */
ZIPOBJ obj;
{  /*  (EZIP must modify byte transfers to be word transfers) 
   */
    ZIPCHAR *objptr;
    ZIPCHAR *objptr2;
    ZIPOBJ i, j;

/* I used GETOID and PUTOID in what is prbably a very inefficent manner
   I think that if the right type assignments were made i = *(objptr + FOO)
   would do the right thing but I just wanted to get on with it   ASK   */

     objptr = objloc(obj);		/* get obj's loc */
    if (i = GETOID(objptr + PARENT))  { /* if there is a parent */
      objptr2 = objloc(i);
      j = GETOID(objptr2 + CHILD);		/* get parent's first */
      if (j == obj) THEN  		/* change to obj's sib */
	PUTOID((objptr2 + CHILD), GETOID(objptr + SIBLING));
      else {				/* get next sib in change */
	do {
	  objptr2 = objloc(j);		/* get obj's loc */
	  j = GETOID(objptr2 + SIBLING);	/* get sib number */
	  if (j == obj) THEN
	    break;
	  }				/* end of do loop */
	while (ZTRUE);			/* while until obj found */
	PUTOID((objptr2 + SIBLING), GETOID(objptr + SIBLING)); /* change */
	}				/* end of else */
      PUTOID((objptr + PARENT), 0);		/* set no parent or sib */
      PUTOID((objptr + SIBLING), 0);
      }					/* end of parent */
    return;
}

printi()		/* OPPRNI prints an immediate string */
{ /*	Since putstr uses GETWRD to advance its pointer, we must update
	the zpc to reflect the advance.
  */
    zblk = zpc1;
    zoff = zpc2;
    putstr();				/* print the string at the zpc */

    zpc2 = zoff;			/* update the zpc */
    if (zpc1 != zblk) THEN {
      zpc1 = zblk;
      newzpc();
      }
    return;
}

printd(obj)
ZIPOBJ obj;
{ /*	Printd prints an object's short description, found at the start
	of its property	table.
  */
    ZIPCHAR *ptr;
    ZIPINT temp;

    ptr = objloc(obj)+PROPS;	/* get obj's property table ptr */
    temp = GTAWRD(ptr); 	/* get (virtual) ptr to props */
    temp++;			/* short desc */
    bspltb(temp);		/* split the ptr */
    putstr();			/* print the string */
    return;
}

jmp_buf icall_env;

zret(rtval) 		/* zret does a OPRETU with value rtval */
ZIPINT rtval;
{
    char doreturn;	/* remember whether we're returning a value */
    short temp;
    zsp = zstack + zlocs;	/* restore old top of stack */
    POPZ();		/* dummy pop */
    temp = POPZ();	/* get back word with # args and retval */
    zargct = temp & 0xff;
    doreturn = (temp >> 8) & 0xff;	/* restore them */
    zlocs = POPZ();	/* restore locals */
    zpc2 = POPZ() & BYTEBITS;	/* restore caller's offset, block */
    zpc1 = POPZ();
    if ((zpc1 == 0) && (zpc2 == 0)) THEN {	/* returning from int call */
      zpc2 = POPZ() & BYTEBITS; /* this is the real return address */
      zpc1 = POPZ();
      newzpc();			/* swap him back in */
      PUSHZ(rtval);		/* leave the return on the stack */
      longjmp(icall_env,1);
      return;
      }
    newzpc();		/* update the pc */
    if (doreturn) THEN		/* don't return unwanted value */
      putval(rtval);
    return;
}

/************************************************************************
*									*
*	O B J E C T   O P E R A T I O N S   S U P P O R T 		*
*									*
************************************************************************/

/* see also: zremove(), printd() above */

ZIPCHAR *objloc(obj)
ZIPOBJ obj;
{  /*   Return a pointer (absolute) to the object obj structure.
	OBJLEN and DPTLEN must be changed for EZIP.
   */
    ZIPINT offset;

    offset = (obj - 1) * OBJLEN;	/* offset of object in objtab */
    offset += objtab + DPTLEN;		/* skipping default prop table */
    return(dataspace + offset);		/* return a real pointer */
}

ZIPINT firstprp(obj)	/* Get 1st property offset for given obj */
ZIPOBJ obj;
{  /*	This routine is shared by the property operations and returns the
	offset from the start of the dataspace for a given object's
	properties.  (EZIP.  Modify for new property definitions.)
   */
    ZIPINT offset, len;
    ZIPCHAR *ptr;

    ptr = objloc(obj) + PROPS;		/* get obj pointer (absolute) */
    offset = GTAWRD(ptr);		/* get prop table offset */
    len = (GTVBYT(offset)) << 1;	/* length of short description */
    return(offset + len + 1);		/* skip over short description */
}

ZIPINT nxtprp(propoff)
ZIPINT propoff;
{  /*	Next property.  This routine take a property offset and wades thru
	the property table to the next property.
   */
    ZIPBYT id, len;

    id = GTVBYT(propoff);		/* get property id */
    len = (id & PSZMSK) >> PROPSIZE;	/* mask, right justify size bits */
    if (len >= 2) {			   /* length follows? */
	len = GTVBYT(propoff + 1) & PNUMSK;  /* length in next byte */
	}
    return(propoff + len + 2);		/* skip extra length byte */
}


ZIPCHAR *flagptr(obj, flagnum)	/* return a pointer to a flag byte in obj */ 
ZIPOBJ obj; 
ZIPINT *flagnum; 
{  /* Flagptr is a common routine to the three flag operations which returns
      a pointer to the correct flag byte (containing flagnum) for obj.
      Also, flagnum is normalized to a value between 0..7, from the possible
	range of 0..31 (0..47 EZIP).
   */
    ZIPINT offset;
    ZIPCHAR *ptr;

    offset = (*flagnum) >> 3;		/* desired flag byte */
    *flagnum = (*flagnum) & 7;		/* desired bit within byte */
    ptr = objloc(obj);
    return(ptr + offset);		/* return adjusted pointer */
}

/************************************************************************
*									*
*	V A R I A B L E   A N D   P R E D I C A T E   S U P P O R T 	*
*									*
************************************************************************/

short getarg(mode)
char mode;
{  /*   Getarg is a general routine called by NXTINS to get arguments for
	an opcode.  It is called with the addressing mode as an parameter
	to determine if the argument should be retrieved as from the stack,
	as immediate data (long or short), global variable or local.
   */
    ZIPBYT result;
    ZIPINT nxtwrd();

    switch (mode) {
      case 0 : return(nxtwrd());	/* long immediate */
      case 1 : return(nxtbyt());	/* short immediate */
      case 2 :
	if (result = nxtbyt()) THEN	/* variable (type detrmd by getvar) */
	  return(getvar(result));
	else
	  return(POPZ());		/* stack */
      default : fatal("Undefined address mode");
      }
}

short getvar(var)
ZIPBYT var;
{  /* 	Getvar retrieves a variable value as dictated by var.  0 indicates
	return tos, 1-15 are local variables referenced through zlocs, and
	16-255 are global.
   */
    ZIPINT global;

    if (var) THEN		/* not a stack variable */
      if (var >= LOCAL) THEN {			/* not local, thus global */
	global = ((var - 16) << 1) + glotab;	/* basify, num*2 + offset */
	return(GTVWRD(global));			/* get the global value */
	}
      else {			/* get a local value */
	--var;
	return(GETLOC(var));
	}
    else
      return(*zsp);		/* return value on top of stack (don't pop) */
}

putvar(var, value)
ZIPBYT var;
short value;
{  /* Sets variable (var) to value.  See above for description of variables */
    ZIPINT global;

    if (var) THEN 		/* not a stack variable */
      if (var >= LOCAL) THEN {			/* not local, thus global */
	global = ((var - 16) << 1) + glotab;	/* basify, num*2 + offset */
	PTVWRD(global, value);			/* set the variable */
	return;
	}
      else {			/* set a local value */
	--var;
	SETLOC(var, value);
	}
    else 
      *zsp = var;		/* update top-of-stack (don't push) */
    return;
}


putval(value)
short value;
{  /*  	Many opcodes return a value.  Putval uses an immediate byte of data
	to determine to what location the value is returned.
   */
    ZIPBYT loc;	/* location to put value */

    loc = nxtbyt();		/* get location indicator */
    loc ? putvar(loc, value) : PUSHZ(value);
    return;
}

bytval(value)
ZIPBYT value;
{  /*  Bytval performs a putval but assures that high bits are off. */
    putval(value & 255);
    return;
}

ppred(truth)		/* do a predicate jump */
ZIPINT truth;
{  /*  	Ppred performs a predicate jump based on truth and immediate values.
	An immediate byte is picked up to determine if the jump is long or
	short or if a return true or false should be done.
   */
    ZIPBYT jump1, jump2; 	/* predicate jump values */
    short offset;
    if (truth) THEN
      truth = 1;		/* truth doesn't have to be 1 */
    jump1 = nxtbyt();		/* get jump value */
    if ((jump1 & BACKWARD) == 0) THEN	/* jump if predicate failed */
      truth++;			/* increment flag */
    if (jump1 & JMPLNTH) THEN 	/* one byte jump offset? */
      offset = jump1 & PREDMSK;	/* mask off special bits */
     else {			/* nope, one byte jump */
      jump1 &= PREDMSK;		/* clear out special bits */
      jump2 = nxtbyt();		/* get low order byte */
      offset = jump1;		/* get high order bits */
      offset = (offset << 8) + jump2;	/* make a word from bytes */
      if (offset & BIT14) THEN	/* is it a 14 bit 2's comp number */
	offset |= COMP16;	/* make into a 16 bit 2's comp */
      }
    if (truth == 1) THEN {/* jump according to truth */
      if (offset != 0) THEN 	/* do jump if there is an offset */
	if (--offset) THEN {	/* do a jump */
	  offset--;		/* adjust offset */
	  zpc2 += offset;	/* add it to pc +++ */
	  newzpc(); 
	  return;
	  }
	else {			/* just return a true */
	  zret(ZTRUE);
	  return;
	  }
      else {
	zret(ZFALSE);		/* just do a return false */
	return;
	}
      }
    return;			/* no jump required */
}

/* to break on function call, set breakaddr to the address (/4) of the
   beginning of the function.  Put a breakpoint at dobreak, which will
   be called when the function in question is. */
static int breakaddr = 0;

void dobreak() {
  return;
}

docall(retval) {
    int ix, jx, temp;
		if (argblk[1] != 0) THEN {
/* Save both halves of the PC, the appropriate stack pointer to restore,
   the number of arguments for the calling routine, and whether the
   calling routine wants this call to return a value. */
		  PUSHZ(zpc1);			/* save return location */
		  PUSHZ(zpc2);
		  PUSHZ(zlocs);			/* save locals */
		  PUSHZ (zargct | (retval << 8));	/* other stuff */
		  bsplitq(argblk[1]);		/* split new code ptr */
		  zargct = argblk[0] - 1;	/* number of args */
		  zpc1 = zblk;
		  zpc2 = zoff;
		  newzpc();			/* update the zpc */
		  zlocs = zsp - zstack;		/* make a locals pointer */
		  zlocs--;		 	/* to next stack slot*/
		  if (breakaddr && (breakaddr == argblk[1])) THEN
		    dobreak();			/* this should cause break */ 
		  ix = nxtbyt();		/* get num locals byte */
		  argblk[0]--;			/* arg[0] has locs to init */
		  jx = 2;			/* index to first opt arg */
		  while (ix-- != 0) {		/* set optional args */
		    if (argblk[0] < 1) THEN 	/* use default */
		      PUSHZ(0);			/* init to 0 in XZIP */
		    else {
		      PUSHZ(argblk[jx]);	/* save arg */
		      jx++;
		      argblk[0]--;		/* dec count of init vals */
		      }				/* end of if optional */
		    }				/* end of while */
		  return;			/* end of real call */
		  }
		else {
		  if (retval)
		    putval(ZFALSE);		/* return a false, maybe */
		  return;
		  }
		}				/* end of opcall */

int internal_call(rout)
  short rout;
 {
  if (rout != 0) THEN {		/* Only do this if routine exists */
    PUSHZ(zpc1);		/* Save the PC */
    PUSHZ(zpc2);
    zpc1 = 0;			/* And set to 0, so return will know */
    zpc2 = 0;
    argblk[0] = 1;		/* Set up fake argblk */
    argblk[1] = rout;
    docall(ZTRUE);		/* Do the call setup */
    if (setjmp(icall_env) == 0) THEN
      main_loop();		/* Run the main loop */
    return(POPZ());		/* Return will leave value on stack */
    }
  else
   return 0;			/* Just return false */
}




/* this will NOT work to return through an internal call (as might
   occur for a timeout routine, or in a CR interrupt).  The horror
   of that is unimaginable. */
do_throw(rval,frame)
ZIPINT rval,frame;
{ if (zlocs > frame) THEN
    fatal("Bad frame to THROW");
  zlocs = frame;		/* restore top of stack */
  zret(rval);			/* and fall into return code */
}
