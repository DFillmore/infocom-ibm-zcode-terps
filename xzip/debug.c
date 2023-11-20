/************************************************************************
*									*
*	D E B U G G E R 						*
*									*
************************************************************************/
#include "zipdefs.h"

#if _DEBUG

#include "struct.h"
#include "extern.h"

debugger()
{
    char c;
    short i;

    if (skipcnt == 0) THEN {
      dump();
      printf("\nZIPDDT>");
      while ((c = getchar()) != 'q') {
	if (c != NO_INPUT) THEN 
	  if (dbgcmd(lc(c))) THEN
	    break;
	  else
	    printf("\nZIPDDT>");
        }				/* end of while */
      }					/* end of if skipcnt */
    skipcnt--;				/* decrement our skip count */
    nxtins();				/* execute the instruction */
}

dump()
{
    if (debug & VERBOSE) THEN {
      printf("\nZPC1 : ZPC2\n");
      printf("%4.4x   %4.4x\n",zpc1 & 0xffff,zpc2);
      }
    return;
}

dinfo(optype, opcode)
ZIPINT optype, opcode;
{
    short i;
    char c;

    last_ins = ++last_ins & 15;		/* continue history list */
    op_hist[last_ins].z1 = zpc1;
    op_hist[last_ins].z2 = zpc2;
    for (i = 0; i < MAXARGS; i++) 
      op_hist[last_ins].argblk[i] = argblk[i];
    op_hist[last_ins].opcode = opcode;
    op_hist[last_ins].opstring = ins_tbl[opcode].opstr;
    PUSH(debug);
    if ((opcode == OPCALL) && (ins_tbl[OPCALL].brkflg == 2) && (argblk[1] == bfunc)) THEN {
      ins_tbl[OPCALL].brkflg |= 1;
      debug = POP();
      debug |= VERBOSE;
      PUSH(debug);
      printf("\nBreakpoint at function at %x", bfunc);
      skipcnt = 0;
      } 
    if ((ins_tbl[opcode].brkflg == 1) || ((z1 == zpc1) && (z2 == zpc2))) THEN {
      debug |= VERBOSE;
      printf("\nBreakpoint at instruction %s\n", ins_tbl[opcode].opstr);
      skipcnt = 0;
      }					/* end of if break point */
    if (debug & VERBOSE) THEN {
      switch (optype) {
	case 0: {
	  printf("\nZero-Op:  %s (%x)", ins_tbl[opcode].opstr, opcode);
	  break;
	  }
	case 1: {
	  printf("\nOne-op: %s (%x) Argument: %x", ins_tbl[opcode].opstr, opcode, argblk[1] & 0xffff);
	  break;
	  }
	case 2: {
	  printf("\nTwo-op: %s (%x) Args: %x  %x", ins_tbl[opcode].opstr, opcode, argblk[1] & 0xffff, argblk[2] & 0xffff);
	  break;
	  }
	case 3: {
	  printf("\nExt-op: %s (%x)\nArgs: ", ins_tbl[opcode].opstr, opcode);
	  for (i = 1; i <= argblk[0]; i++)
	    printf("%x ",argblk[i] & 0xffff);
	  break;
	  }
	default: {
	  printf("\nUndefined opcode %x", opcode);
	  break;
	  }
	}
      dump();
      }
    debug = POP();
    if (ins_tbl[opcode].brkflg & 1) THEN {
      printf("\nZIPDDT>");
      while ((c = getchar()) != 'q') 
	if (c != NO_INPUT) 
	  if (dbgcmd(lc(c))) THEN
	    break;
	  else
	    printf("\nZIPDDT>");
      }
    return;
}




dbgcmd(c)
char c;
{
    char brkflg = 0;
    int brkins, i, j;

    switch (c) {
      case SPACE: {
	debug = (debug & SKIPS) | STEP;
	skipcnt = 1;
	brkflg = 1;
	break;
	}
      case 'h' : {
	printf("\n<SPACE>\t- Single step one instruction"); 
	printf("\na\t- Dump absolute page number"); 
	printf("\nb\t- Set a break point");
	printf("\nc\t- Clear break point at instruction");
	printf("\nd\t- Disable all break points"); 
	printf("\ne\t- Enable all break points"); 
	printf("\nf\t- Turn off debugger");
	printf("\ng\t- Go (proceed until break point)");
	printf("\nh\t- This message");
	printf("\nj\t- Set breakpoint at function)"); 
	printf("\nl\t- Look at Local or Global variable"); 
	printf("\nm\t- Set a memory location");
	printf("\no\t- Show input and output buffers"); 
	printf("\ns\t- Skip n instructions");
	printf("\nv\t- Toggle verbosity flag");
	printf("\nw\t- Set a Local or Global variable"); 
	printf("\nx\t- Dump a virtual block of data");
	printf("\nz\t- Set breakpoint at ZPC1:ZPC2");
	printf("\n^C\t- To exit program from debugger");
	break;
	}
      case 'j': {
 	printf("\nSet breakpoint at function (word pointer): ");
	bfunc = getnum(HEX);	
	if (bfunc) THEN
	  ins_tbl[OPCALL].brkflg = 2;
	else
	  ins_tbl[OPCALL].brkflg = 0;
	break;
	}
      case 'v': {
	debug ^= VERBOSE;
	if (debug & VERBOSE) THEN
	  printf("\nVerbosity turned ON.");
	else
	  printf("\nVerbosity turned OFF.");
	break;
	}
      case 'b': {
	printf("\nSet breakpoint at instruction: ");
 	brkins = getnum(DEC);
	ins_tbl[brkins].brkflg = 1;
	printf("Breakpoint set at instruction %s", ins_tbl[brkins].opstr);
	break;
	}
      case 'c': {
	printf("\nClear breakpoint at instruction: ");
	brkins = getnum(DEC);
	if (brkins) THEN 
	  ins_tbl[brkins].brkflg = 0;
	else {
	  for (i = 0; i <= 255; i++) 
	    ins_tbl[i].brkflg = 0;
	  }
	break;
	}
      case 'd': {
	debug &= ~BRKPT;
	break;
	}
      case 'e': {
	debug |= BRKPT;
	break;
	}
      case 'f': {
	debug = OFF;		/* turn off debugger */
	brkflg = 1;
	break;
	}
      case 'g': {
	skipcnt = -1;
	brkflg = 1;
	break;
	}
      case 'l': {
	printf("\nLook at value of Global/Local: ");
	i = getnum(HEX);
	printf("Word value is: %x", getvar(i));
	break;
	}
      case 'm': {
	printf("\nSet memory location: ");
	i = getnum(HEX);
	printf("Word value is: %x", GTVWRD(i));
	printf("\nSet to value: ");
	j = getnum(HEX);
	PTVWRD(i, j);
	break;
	}
      case 'o': {
	printf("\nThe current output buffer is:\n%s", outbuf);
	printf("\nThe character pointer starts at:\n%s", chrptr);
	printf("\n\The input buffer is:\n%s", inbuf);
	break;
	}
      case 's': {
	printf("\nExecute the next n instructions: ");
        skipcnt = getnum(DEC);
	if (skipcnt == 0) THEN
	  skipcnt = 1;
	brkflg = 1;
	break;
	}
      case 'a': {
	printf("\nDump absolute block number: ");
 	i = getnum(HEX);	
	adump(i);
	break;
	}
      case 'w': {
	printf("\nLocal/Global to set: ");
	i = getnum(HEX);
  	printf("Current value is: %x", getvar(i));
	printf("\nSet variable to value: ");
	j = getnum(HEX);
	putvar(i, j);
	break;
	}
      case 'x': {
	printf("\nDump virtual block number: ");
	i = getnum(HEX);
	vdump(i);
	break;
	}
      case 'z': {
	printf("\nSet breakpoint at ZPC1: ");
	z1 = getnum(HEX);
	printf("Set breakpoint at ZPC2: ");
	z2 = getnum(HEX);
	debug |= BRKPT;
	break;
	}
      case 3: z_exit();		/* ^C to return to UNIX */
      default: printf("\nUndefined debug command\n");
      }				/* end of switch */
    return(brkflg);
}

getnum(radix)
int radix;
{
    char control[3], numstr[10];
    int num = 1;

    md_getl(numstr, 10);
    sprintf(control, "%%%c", radix);
    sscanf(numstr, control, &num);
    return(num);
}

vdump(blknum)		/* dump a block of data formatted */
short blknum;
{
    ZIPCHAR *loc;

printf("\nBlock number goes in as %d", blknum);
    if (blknum < endlod) THEN
      loc = (dataspace + (blknum << CVTBLK));
    else 
      loc = pagemap[blknum]->loc;
    if (loc >= dataspace) THEN
      ddump(loc);
    else
      printf("\nLocation %d for block number %d is not valid",loc,blknum);
    return;
} 

adump(blknum)
short blknum;
{
    if ((blknum >= 0) && (blknum <= MAXBLKS)) THEN {
      printf("\nTrying to dump block %d at %d",blknum,dataspace+(blknum<<CVTBLK));
      ddump(dataspace + (blknum << CVTBLK));
      }
    else
      printf("\Invalid block number %d", blknum);
    return;
}

ddump(loc)
ZIPCHAR *loc;
{
    short i , j;
    ZIPCHAR *tmp;
  
    printf("\nBase: %6.6x", loc); 
    for (i = 0; i < BLKSIZ; i++) {
      if (((i % 8) == 0) && (i % 16)) THEN
	printf(" -");
      if (i % 16) THEN 
	printf(" ");
      else {
	printf("\n[%3.3x] ", i);
	tmp = loc + i;
	}
      printf("%-2.2x",*(loc+i) & 255);
      if ((i % 16) == 15) THEN { 
	printf("    [");
	for (j = 0; j <= 15; j++) 
	  if ((*(tmp+j) >= SPACE) && (*(tmp+j) <= 127)) THEN
	    printf("%c", *(tmp+j));
	  else
	    printf(".");
	printf("]");
	}
      }
    return;
} 
#endif
