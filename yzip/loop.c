/************************************************************************
*                                                                       *
*  I N S T R U C T I O N   D I S P A T C H                              *
*                                                                       *
************************************************************************/

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"
#include "sysdep.h"

#include <setjmp.h>
#include <dos.h>

static jmp_buf icall_env;

void main_loop()
{
   /*   MAIN_LOOP is the heart of ZIP.  It picks up the next instruction
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

   ZIPINT opcode;
   ZIPBYT operand;
   char adrmode;

   /*   Efficiency note: should avoid unnecessary initializing of automatic 
   vars. 
   */

/* split up into different cases to give optimizer a chance... */

   while ( ZTRUE )
   { /* we only ever longjmp out of here */
      opcode = nxtbyt();         /* get opcode byte */

      if (opcode == OPEXTOP)
      { /* handle two-byte opcodes */
         opcode = 256 + nxtbyt();
      }
      if (opcode)
      { /* legal operation */
         if (opcode < ONE_OP)
         { /* it's a two op */
            if ( (operand = (ZIPBYT)(opcode & TWOMSK)) != 0)
            { /* isolate operand */
               adrmode = 1;         /* addressing mode immediate */
               if (opcode & TWOMOD1)    /* check for variable arg */
                  adrmode++;
               argblk[1] = getarg(adrmode);   /* get argument by adrmode */
               adrmode = 1;         /* reset to immediate */
               if (opcode & TWOMOD2)    /* check for variable arg */
                  adrmode++;
               argblk[2] = getarg(adrmode);  /* get second arg by adrmode */
               argblk[0] = 2;                /* show just 2 args */
               run_two_op(operand);
            }
            else
            { /* else it's not a valid TWO */
               fatal("Undefined 2-op");
            }
         }   
         else if (opcode < ZERO_OP)
         { /* one op */
               operand = (ZIPBYT)(opcode & ONEMODE) >> 4;   /* mode bits */
               opcode = (opcode & ONEMSK) + ONE_OP;   /* operator bits */
               argblk[1] = getarg(operand);      /* get the arg */
               run_one_op( opcode );
         }
         else if (opcode < EXT_OP)
         { /* zero op */
            opcode = (opcode & ZEROMSK) + ZERO_OP;
            run_zero_op( opcode );
         }
         else
         { /* extended op */
            run_ext_op(opcode);
         }
      }
      else
      {
         fatal("Undefined operation");
      }
   }
}

/* dispatch for two ops */
void run_two_op(operand)
   ZIPBYT operand;
{
   short ts1;
   ZIPCHAR *ptr;
   ZIPCHAR *objptr2;
   ZIPCHAR *objptr;
   ZIPBYT t1,t2,id;
   ZIPOBJ objx;
   ZIPINT temp;

   switch (operand)
   { /* find two op */
   case OPQEQU:                  /* EQUAL? */
      PRED(argblk[1] == argblk[2]);
      break;
   case OPQLES:                  /* LESS? */
      PRED(argblk[1] < argblk[2]);
      break;
   case OPQGRT:                   /* GREATER? */
      PRED(argblk[1] > argblk[2]);
      break;
   case OPQDLE:                   /* DECREMENT LESS? */
      ts1 = getvar( (ZIPBYT)argblk[1]);   /* get variable */
      putvar( (ZIPBYT)argblk[1],--ts1);   /* set dec'ed var */
      PRED(ts1 < argblk[2]);
      break;
   case OPQIGR:                   /* INCREMENT GREATER? */
      ts1 = getvar( (ZIPBYT)argblk[1]);
      putvar( (ZIPBYT)argblk[1],++ts1);
      PRED(ts1 > argblk[2]);
      break;
   case OPQIN:                   /* IN? */
      objptr = objloc(argblk[1]);
      PRED((GETOID(objptr+PARENT) == OBJARG(2)));
      break;
   case OPBTST:       
      PRED((~argblk[1] & argblk[2]) == 0);
      break;
   case OPBOR:
      putval(argblk[1] | argblk[2]);
      Scrchk = ZTRUE;      /* instruction used to toggle script */
      break;
   case OPBAND:
      putval(argblk[1] & argblk[2]);
      Scrchk = 1;      /* check scripting bit */
      break;
   case OPQFSE:                  /* FlagSET? */
      ptr = flagptr( argblk[1], (ZIPINT *)&argblk[2] );
      t1 = BIT8;
      PRED(GTABYT(ptr) & (t1 >> argblk[2]));
      break;
   case OPFSET:                   /* FlagSET */
      ptr = flagptr( argblk[1], (ZIPINT *)&argblk[2]);
      t1 = BIT8;
      t2 = GTABYT(ptr) | (t1 >> argblk[2]);
      PTABYT(ptr, t2);
      break;
   case OPFCLE:                   /* FlagCLEAR */
      ptr = flagptr( argblk[1], (ZIPINT *)&argblk[2]);
      t1 = BIT8;
      t2 = GTABYT(ptr) & (~(t1 >> argblk[2]));
      PTABYT(ptr, t2);
      break;
   case OPSET:
      putvar( (ZIPBYT)argblk[1],argblk[2]); 
      break;
   case OPMOVE:
      zremove(argblk[1]);               /* remove object from cont */
      objptr = objloc(argblk[1]);      /* find loc of obj2 */
      objptr2 = objloc(argblk[2]);      /* and that of obj1 */
      PUTOID(objptr+PARENT, OBJARG(2)); /* obj2 into obj1's loc*/
      objx = GETOID(objptr2+CHILD);      /* get contents of 2's 1st */
      PUTOID(objptr2+CHILD, OBJARG(1)); /* obj1 now 1st in obj2*/
      if (objx != EMPTY)                /* chain into sibling */
           PUTOID(objptr+SIBLING, objx);   /* yes, chain into 1's sib */
      break;
   case OPGET:
      argblk[2] <<= 1;
      argblk[1] += argblk[2];    /* make an index into table*/
      putval(datwrd(argblk[1]));      /* return the word */
      break;
   case OPGETB:
      argblk[1] += argblk[2];      /* make an index into table*/
      bytval(datbyt(argblk[1]));      /* return the byte */
      break;
   case OPGETP:
      temp = find_prop( argblk[1], (ZIPBYT)argblk[2], &id );

      if ( id < BYTARG(2) )
      { /* wasn't found, sorry! */
         temp = ((--argblk[2]) << 1) + objtab; /* use default */
      }
      else if ( (GTVBYT(temp++) & PSZMSK) == 0)
      { /* id == Arg2, so we found it, and it is a byte property */
         bytval(GTVBYT(temp));
         break;
      }
      putval(GTVWRD(temp));      /* return the word */
      break;
   case OPGTPT:               /* GET PROPERTY TABLE */
      temp = find_prop( argblk[1], (ZIPBYT)argblk[2], &id );

      if ( id == BYTARG(2) )
      { /* got it */
         if (GTVBYT(temp) & 0x80 )
         { /* skip over the length byte then */
            temp++;   /* length byte? */
         }
         temp++;               /* skip over id */
      }   
      else
      { /* no such prop */
         temp = 0;
      }
      putval(temp);      /* and return the property pointer */
      break;
   case OPNEXT:
      temp = find_prop( argblk[1], (ZIPBYT)argblk[2], &id );
      if ( BYTARG(2) && id < BYTARG(2) )
      { /* property was there and not found */
         fatal("Property not found.");
      }
      putval((GTVBYT(temp) & PNUMSK));/* return the property */
      break;
   case OPADD: 
      putval(argblk[1] + argblk[2]);
      break;
   case OPSUB:
      putval(argblk[1] - argblk[2]);
      break;
   case OPMUL: 
      putval(argblk[1] * argblk[2]);
      break;
   case OPDIV:
      putval(argblk[1] / argblk[2]);
      break;
   case OPMOD:
      putval(argblk[1] % argblk[2]);
      break;
   case OPCALL2:
      argblk[0] = 2;
      docall(ZTRUE);
      break;
   case OPICALL2:
      argblk[0] = 2;
      docall(ZFALSE);         /* says don't return a value */
      break;
   case OPCOLOR:
      flush_buffer();
      if ( argblk[0] <= 2 )
      { /* no window passed, so use current window */
         argblk[3] = Screen;
      }
      md_set_color( argblk[1], argblk[2], argblk[3]);
      break;
   case OPTHROW:
      do_throw(argblk[1],argblk[2]);
      break;
   default:
      fatal("Undefined 2-op");
   }
}

/* Look for 'property' in 'object'.  Return offset to last property
looked at
*/
int find_prop( object, property, lastp )
   short object;         /* starting object */
   ZIPBYT property;      /* which property am I looking for? */
   ZIPBYT *lastp;        /* last one I found (hopefully == property) */
{
   unsigned short temp;

   temp = firstprp( object );   /* get property pointer */
   do
   {
     *lastp = GTVBYT(temp) & PNUMSK;      /* get property id */
   } while ( (property < *lastp) && ((temp = nxtprp(temp)) != 0) );
   return( temp );
}

void get_ext_args(opcode)
   ZIPINT opcode;
{
   int jx = 0;
   int temp, ashift;
   ZIPBYT adrmode, adrmode2;
   int ax = 1;

   adrmode = nxtbyt();
   if ((opcode == OPXCALL) || (opcode == OPIXCALL))
   { /* get second mode byte */
      adrmode2 = nxtbyt();
      ax++;
   }

   while( ax-- )
   { /* pick up (possibly two bytes worth) arguments */
      for ( ashift = 6; ashift >= 0; ashift -= 2)
      { /* do up to 4 possible operands in this byte */
         if ((temp = ((adrmode >> ashift) & 3)) == 3) 
            break;
         argblk[++jx] = getarg( (char)temp );
      }
      adrmode = adrmode2;           /* maybe do xcall args */
   }
   argblk[0] = jx;                  /* tell how many args */
}

void run_ext_op(opcode)
   ZIPINT opcode;
{
   ZIPBYT ix,id;
   ZIPINT operand;
   ZIPINT temp;

   if (opcode < 256)
      opcode = (opcode & EXTMSK) + EXT_OP;      /* actual opcode */

   get_ext_args(opcode);

   switch (opcode)
   {
   case XQEQU:                /* EXTENDED EQUAL? */
      for (ix = 2; ix <= BYTARG(0); ix++)
      { /* check all the arguments for an equal with the first */
         if (argblk[1] == argblk[ix])
         { /* a match!!!! */
            ppred(ZTRUE);      
            return;
         }
      }
      ppred(ZFALSE);         /* no match found */
      break;
   case OPCALL:
      docall(ZTRUE);
      break;
   case OPICALL:
      docall(ZFALSE);
      break;
   case OPPUT:
      argblk[2] <<= 1;                 /* convert words to bytes */
      argblk[1] += argblk[2];          /* add in offset */
      PTVWRD(argblk[1],argblk[3]);     /* put word arg3 @offset */
      break;
   case OPPUTB:
      argblk[1] += argblk[2];          /* figure index */
      PTVBYT(argblk[1],BYTARG(3));     /* return byte */
      break;
   case OPPUTP:
      temp = find_prop( argblk[1], BYTARG(2), &id );
      if (id < BYTARG(2))  
         fatal("Property not found");   /* bye-bye ... */

      id = (GTVBYT(temp++)) & PSZMSK;   /* now find length */
      if ((id >>= PROPSIZE) == 0)
      { /* must be just a byte, thank you */
         PTVBYT(temp,BYTARG(3));
      }
      else
      { /* so stash the whole word */
         PTVWRD(temp, argblk[3]);   /* and return the prop */
      }
      break;
   case OPREAD:
      putval(zread());      /* xzip read returns a value */
      break;
   case OPLEX:                   /* interface to LEX */
      if (argblk[0] < 4)
      { /* default the arguments */
         argblk[4] = 0;         /* preserve argument */
         if (argblk[0] < 3) 
            argblk[3] = 0;         /* lexicon argument */
      }
      zlex(argblk[1],argblk[2],argblk[3],argblk[4]);
      break;
   case OPZWSTR:
      zwstr(argblk[1],argblk[2],argblk[3],argblk[4]);
      break;
   case OPPRNC:                  /* PRINT CHAR */
      putchr(argblk[1]);
      break;
   case OPPRNN:                  /* PRINT NUMBER */
      print_number(argblk[1],putchr);
      break;
   case OPRAND:                  /* RANDOM */
      dorand();
      break;
   case OPPUSH:
      PUSHZ(argblk[1]); 
      break;
   case OPPOP:
      pop(argblk[1]);
      break;
   case OPSPLT:                  /* SPLIT */
      dosplit(argblk[1]);
      break;
   case OPSCRN:                  /* SCREEN */
      newdoscreen(argblk[1]);
      break;
   case OPXCALL: 
      docall(ZTRUE);
      break;
   case OPIXCALL:
      docall(ZFALSE);
      break;
   case OPCLEAR: 
      flush_buffer();
      if ( argblk[1] == -1 || argblk[1] == -2 )
      { /* then do a split too */
			if ( argblk[1] == -1 )
         	dosplit( 0 );
			clear_screen();
      }
      else
      { /* must be a useful window, so pass it */
         md_clr( argblk[1] );
      }
      break;
   case OPERASE:
      flush_buffer();
		md_erase( argblk[1] );
      break;
   case OPCURSET:
      flush_buffer();
      if ( argblk[0] == 2 )
      { /* use current window if only 2 args */
         argblk[3] = Screen;
      }
      curset(argblk[1],argblk[2],argblk[3]);
      break;
   case OPHLIGHT:
		putchr( (argblk[1] | 0xC0) );
      break;
   case OPBUFOUT:
         flush_buffer();
#ifdef OLD_STUFF
      { /* 0->off,else on */
         for ( inum=0; inum <= 7; inum++)
         { /* set buffer flag of window to zero */
            winattr(inum,BUFFERMASK,2);   
            flush_buffer();
         }
      }
      else
      {
         for ( inum=0; inum <= 7; inum++ )
         { /* set buffer flag of window to one */
            winattr(inum,BUFFERMASK,1);
            flush_buffer();
         }
      }
#endif
      break;
   case OPDIROUT:
      dodirout( argblk[1], argblk[2], argblk[3], (argblk[0]>2) );
      break;
   case OPINPUT:
      flush_buffer();   /* flush buffered output */
      if (argblk[0] == 3)   /* time out version? */
         bytval( (ZIPBYT)md_tinp(argblk[2],argblk[3]));
      else
         bytval( (ZIPBYT)md_inp());      /* wait for chr */
      break;
   case OPSOUND:      /* 1 - beep, 2 - boop */
      switch(argblk[1])
      {
      case 1:  
         md_sound(BEEP);
         break;
      case 2:
         md_sound(BOOP);
         break;
      default:
         fatal("undefined sound");
      }
      break;
   case OPDIRIN:   /* input from kb (0) or script file */
      /* unimplemented */
      break;
   case OPCURGET:   /* get cursor */
      flush_buffer();
      globals_to_window( Screen );
		/* taa 6/22/89  Add one on output, since 0-based internally
			and 1-based externally */
      PTVWRD(argblk[1], wattrib[Screen].ycurpos + 1);
      PTVWRD(argblk[1]+2, wattrib[Screen].xcurpos + 1);
      break;
   case OPMARGIN:
      if ( argblk[0] < 3 )
      { /* no window ID, default to current screen */
         argblk[3] = Screen;
      }
      domargin(argblk[1],argblk[2],argblk[3]);
      break;
   case OPINTBL:
      if (argblk[0] > 3) 
         do_intbl(argblk[1],argblk[2],argblk[3],argblk[4]);
      else
         do_intbl(argblk[1],argblk[2],argblk[3],0202);
      break;
   case OPASHIFT:            /* arithmetic shift */
      if (argblk[2] < 0) 
         putval(argblk[1] >> -argblk[2]);   /* always arithmetic shift */
      else               /* since argblk is signed */
         putval(argblk[1] << argblk[2]);
      break;
   case OPSHIFT:            /* logical shift */
      if (argblk[2] > 0) 
         putval(argblk[1] << argblk[2]);   /* right shift is OK */
      else
         putval (((unsigned) argblk[1]) >> -argblk[2]);
      break;
   case OPASSNQ:            /* assigned? */
      PRED(argblk[1] <= zargct);      /* this guy was an argument */
      break;
   case OPBCOM:            /* complement */
      putval(~argblk[1]);
      break;
   case OPCOPYT:
      do_table_copy(argblk[1],argblk[2],argblk[3]);
      break;
   case OPPRINTT:
      if (argblk[0] < 4)
      { /* default optionals */
         argblk[4] = 0;
         if (argblk[0] < 3) 
            argblk[3] = 1;
      }
      do_table_print(argblk[1],argblk[2],argblk[3],argblk[4]);
      break;
   case OPXSAVE:
      if (argblk[0] == 0)
         putval(sav_res(OPSAVE));
      else
         putval(partial_sav_res(OPSAVE,argblk[1],argblk[2],argblk[3]));
      break;
   case OPXRESTORE:
      if (argblk[0] == 0) 
         putval(sav_res(OPREST));
      else
         putval(partial_sav_res(OPREST,argblk[1],argblk[2],argblk[3]));
      break;
   case OPISAVE:
      if (!Isave_allowed) 
         putval(-1);         /* user disabled it, or no memory */
      else
         putval(internal_sav_res(OPSAVE));
      break;
   case OPIRESTORE:
      if (!Isave_happened) 
         putval(0);         /* save never done successfully */
      else
         putval(internal_sav_res(OPREST));
      break;
   case OPWINPOS:   /* #272 */
      flush_buffer();
      winpos(argblk[1],argblk[2],argblk[3]);
      break;
   case OPWINSIZE:   /* #273 */
      flush_buffer();
      winsize(argblk[1],argblk[2],argblk[3]);
      break;
   case OPWINATTR:   /* #274 */
      flush_buffer();
      winattr(argblk[1],argblk[2],argblk[3]);
      break;
   case OPWINGET:   /* #275 */
      flush_buffer();
      putval(winget(argblk[1],argblk[2]));
      break;
   case OPSCROLL:   /* #276 */
      flush_buffer();
      if (argblk[0] == 1)
      { /* default to one line to scroll */
         argblk[2] = 1;
      }

      scroll(argblk[1],argblk[2]);
      break;
   case OPWINPUT:   /* #281 */
      flush_buffer();
      winput(argblk[1],argblk[2],argblk[3]);
      break;
   case OPPRINTF:   /* #282 */
      flush_buffer();
      prt_tblf(argblk[1]);
      break;
   case OPMSEINFO:
      mouse_info(argblk[1]);
      break;
   case OPMSELIM:
      mouse_lim(argblk[1]);
      break;
   case OPXPUSH:
      xpush(argblk[1],argblk[2]);
      break;
   case OPFSTACK:
      fstack(argblk[1],argblk[2]);
      break;
   case OPFONT:
      flush_buffer();
      if ( argblk[0] < 2 )
      { /* make window be current screen */
         argblk[2] = Screen;
      }
      putval(md_set_font(argblk[1], argblk[2]) );
      break;
   case OPPICINF:
      picinf(argblk[1], argblk[2]);
      break;
   case OPDISPLAY:
      disp_pic(argblk[1],argblk[2],argblk[3]);
      break;
	case OPMENU:
      ppred(ZFALSE);         /* no match found */
		break;
   case OPPICSET:                /* not implemented (yet . . .) */
   case OPDCLEAR:
      break;
   default:
      if ((operand = opcode - EXT_OP) <= LAST_TWO_OP)
         run_two_op(operand);
      else      
         fatal("Undefined Ext-Op");
   }               /* end of switch statement */
}



/* Do OPRAND */
void dorand()
{
   int temp;

   if (argblk[1] <= 0)
   { /* enable/disable */
      rcycle = -argblk[1];      /* save abs value */
      putval(rconst = rcycle);
   }
   else if (rcycle != 0)
   { /* already disabled */
      if (++rconst > rcycle)
         rconst = 1;
      putval(rconst);
   }
   else
   { /* normal */
      argblk[1] &= BYTEMSK;      /* argument <= 255 */
      temp = rand();
      putval((temp % argblk[1]) + 1);
   }
}

/* OPDIROUT */
void dodirout( opcode, tbladr, tblwid, formflag )
   int opcode, tblwid;
   ZIPINT tbladr;      /* addr. of table */
	int formflag;
{
   ZIPINT temp;

   switch( opcode & 0xFF ) 
   {
   case 1:      /* screen */
      Vidflg = ZTRUE;
      break;
   case 2:      /* script */
      if ( !Scripting && !Scrhld )
      { /* we aren't already Scripting and we're allowed to */
         temp = GTVWRD(PFLAGS);
         PTVWRD(PFLAGS, temp | SCRIPTBIT);
         chkscript();
      }
      break;
   case 3:      /* table */
      Table = tbladr;                 /* namely this one */

      if ( formflag )
      { /* doing formatted table */
         if ( tblwid < 0 )
            FTblwidth = -tblwid;
         else
            FTblwidth = (wattrib[tblwid].xsize - wattrib[tblwid].rmarg -
               wattrib[tblwid].lmarg);
         FTblout = ZTRUE;
			Tblptr = Table;
      }
      else 
      { /* turn off formatted table stuff */
         PTVWRD( PTWIDTH, 0 );			/*  init twidth counter */
         Tblout = ZTRUE;
     		Tblchars = 0;
      	Tblptr = Table + 2;             /* point to start of data */
      }
      break;
   case 4:      /* file not supported */
      break;
   case 0xFF:      /* screen off */
      flush_buffer();
      Vidflg = ZFALSE;
      break;
   case 0xFE:      /* script off */
      temp = GTVWRD(PFLAGS);
      PTVWRD(PFLAGS, temp & (~SCRIPTBIT));
      chkscript();
      break;
   case 0xFD:      /* table off */
		if ( !FTblout )
		{ /* show how many chars went in there */
      	PTVWRD( Table, Tblchars );
		}
		else
		{ /* just zero formatted table */
			dumpbuf();
      	PTVWRD( Tblptr, 0x0 );
		}
      FTblout = Tblout = ZFALSE;
      break;
   case 0xFC:
      break; 
   }
}

/* OPSPLIT */
void run_zero_op( code )
   ZIPBYT code;
{
   switch (code)
   {
   case OPRTRU:                           /* RTRUE */
      zret(ZTRUE);
      break;
   case OPRFAL:                           /* RFALSE */
      zret(ZFALSE);
      break;
   case OPPRNI:                           /* PRINT IMMEDIATE STRING */
      printi();
      break;
   case OPPRNR:                           /* PRINT WITH CRLF */
      printi();         /* print immediate string */
      newlin();         /* with a carriage return */
      zret(ZTRUE);      /* and return a true */
      break;
   case OPNOOP:
   case OPUSL:                            /* STATUS LINE (no-op in YZIP) */
      break;
   case OPSAVE:                           /* SAVE with return */
      if( sav_res(OPSAVE) )
         putval(1);        /* save ok */
      else
         putval(0);        /* save failed */
      break;
   case OPREST:                           /* RESTORE with return */
      if( sav_res(OPREST))
         putval(2);        /* restore ok */
      else
         putval(0);        /* restore failed */
      break;
   case OPRSTT:                           /* RESTART */
      newlin();            /* cr and flush buffer */
      restart(ZTRUE);      /* midgame restart */
      break;
   case OPRSTA:                        /* RETURN VALUE ON STACK */
      zret(POPZ());
      break;
   case OPCATCH:           /* return a "frame pointer" */
      putval(zlocs);       /* this will get back to TOS */
      break;
   case OPQUIT:                        /* QUIT */
      z_exit( 0 );         /* hasta lumbago, guys */
      break;
   case OPCRLF:                        /* CRLF */
      newlin();
      break;
   case OPVERI:                        /* VERIFY */
      PRED(verify());
      break;
   default:
      fatal("Undefined 0-op");
   }
}

void run_one_op( code )
   ZIPBYT code;
{

   ZIPBYT id;
   ZIPCHAR *objptr;
   ZIPOBJ objx;
   long temp1;
   unsigned short temp2;

   switch (code)
   {
   case OPQZER:                        /* ZERO? */
      PRED(argblk[1] == 0);
      break;
   case OPQNEX:                        /* NEXT? */
      objptr = objloc(argblk[1]);   /* get location */
      objx = GETOID(objptr + SIBLING); /* get sib slot */
      OBJVAL(objx);        /* return the byte value */
      PRED(objx);          /* predicate return */
      break;
   case OPQFIR:                        /* FIRST? */
      objptr = objloc(argblk[1]); /* get obj's location */
      objx = GETOID(objptr + CHILD);   /* get child slot */
      OBJVAL(objx);        /* return the byte value */
      PRED(objx);          /* predicate return */
      break;
   case OPLOC:                         /* LOC */
      objptr = objloc(argblk[1]); /* get obj's location */
      objx = GETOID(objptr + PARENT);   /* get obj's parent */
      OBJVAL(objx);         /* return byte value */
      break;
   case OPPTSI:                        /* PROPERTY TBL SIZE */
      id = GTVBYT(argblk[1]-1);   /* get property id */
      id &= PSZMSK;           /* isolate size bits */
      id >>= PROPSIZE;        /* right justify bits */
      if (id >= 2)
      { /* variable length? */
         id = GTVBYT(argblk[1]-1) & PNUMSK;
      }
      else
      { /* length bits */
         id++;      /* 0 => 1 byte, 1 => 2 bytes */
      }
      bytval(id);
      break;
   case OPINC:                         /* INC */         
      putvar( (ZIPBYT)argblk[1], (getvar( (ZIPBYT)argblk[1]) + 1));
      break;
   case OPDEC:                         /* DEC */
      putvar( (ZIPBYT)argblk[1], (getvar( (ZIPBYT)argblk[1]) - 1));
      break;
   case OPPRNB:                        /* PRINT BYTE ALIGNED STRING */
      bspltb(argblk[1]);   /* get string pointer */
      putstr();
      break;
   case OPCALL1:                       /* CALL1 */
      argblk[0] = 1;
      docall(ZTRUE);
      break;
   case OPICALL1:                      /* ICALL1 */   
      argblk[0] = 1;
      docall(ZFALSE);
      break; 
   case OPREMO:                        /* REMOVE */
      zremove(argblk[1]);
      break;
   case OPPRND:                        /* PRINT OBJ'S DESCRIPTION */
      printd(argblk[1]);      /* print short desc */
      break;                  /* and go away */
   case OPRETU:                        /* RETURN */
      zret(argblk[1]);
      break;
   case OPJUMP:                        /* JUMP */
      zpc2 += argblk[1] - 2;   /* offset - normalize */
      newzpc();
      break;
   case OPPRIN:                        /* PRINT QUAD ALIGNED STRING */
      temp2 = (unsigned short)(argblk[1]);
      temp1 = (temp2 + (SOFF8/4));
      bsplitq2(temp1);
      putstr();
      break;
   case OPVALU:                       /* VALUE */
      putval(getvar( (ZIPBYT)argblk[1]));
      break;
   default:
      fatal("Undefined 1-op");
   }            /* end of switch statement */
}
/************************************************************************
*                                                                       *
*   S H A R E D   O P C O D E S                                         *
*                                                                       *
************************************************************************/

void zremove(obj)       /* OPREMO separated for use by OPMOVE */
ZIPOBJ obj;
{  
   ZIPCHAR *objptr;
   ZIPCHAR *objptr2;
   ZIPOBJ i, j;

   objptr = objloc(obj);      /* get obj's loc */
   if ( (i = GETOID(objptr + PARENT)) != 0)
   { /* if there is a parent */
      objptr2 = objloc(i);
      j = GETOID(objptr2 + CHILD);     /* get parent's first */
      if (j == obj)                    /* change to obj's sib */
         PUTOID((objptr2 + CHILD), GETOID(objptr + SIBLING));
      else
      { /* get next sib in change */
         do
         {
            objptr2 = objloc(j);      /* get obj's loc */
            j = GETOID(objptr2 + SIBLING);   /* get sib number */
         } while ( j != obj );         /* while until obj found */
         PUTOID((objptr2 + SIBLING), GETOID(objptr + SIBLING)); /* change */
      }
      PUTOID((objptr + PARENT), 0);      /* set no parent or sib */
      PUTOID((objptr + SIBLING), 0);
   }
}

void printi()      /* OPPRNI prints an immediate string */
{ /*   Since putstr uses GETWRD to advance its pointer, we must update
   the zpc to reflect the advance. */

   zblk = zpc1;
   zoff = zpc2;
   putstr();            /* print the string at the zpc */

   zpc2 = zoff;         /* update the zpc */
   if (zpc1 != zblk)
   {
      zpc1 = zblk;
      newzpc();
   }
}

void printd(obj)
   ZIPOBJ obj;
{ /*   Printd prints an object's short description, found at the start
   of its property   table.
   */
   ZIPCHAR *ptr;
   ZIPINT temp;

   ptr = objloc(obj)+PROPS;      /* get obj's property table ptr */
   temp = GTAWRD(ptr);           /* get (virtual) ptr to props */
   temp++;                       /* short desc */
   bspltb(temp);                 /* split the ptr */
   putstr();                     /* print the string */
}

void zret(rtval)                 /* zret does a OPRETU with value rtval */
   ZIPINT rtval;
{
   char doreturn;                /* remember whether we're returning a value */
   short temp;

   zsp = zstack + zlocs;         /* restore old top of stack */
   POPZ();                       /* dummy pop */
   temp = POPZ();                /* get back word with # args and retval */
   zargct = (char)temp & 0xff;
   doreturn = (char)(temp >> 8) & 0xff;   /* restore them */
   zlocs = POPZ();               /* restore locals */
   zpc2 = POPZ() & BYTEBITS;     /* restore caller's offset, block */
   zpc1 = POPZ();
   if ((zpc1 == 0) && (zpc2 == 0))
   { /* returning from int call */
      zpc2 = POPZ() & BYTEBITS;  /* this is the real return address */
      zpc1 = POPZ();
      newzpc();                  /* swap him back in */
      PUSHZ(rtval);              /* leave the return on the stack */
      longjmp(icall_env,1);      /* and run away */
   }
   newzpc();                     /* update the pc */
   if (doreturn)                 /* don't return unwanted value */
      putval(rtval);
}

/************************************************************************
*                                                                       *
*   O B J E C T   O P E R A T I O N S   S U P P O R T                   *
*                                                                       *
************************************************************************/

/* see also: zremove(), printd() above */

ZIPCHAR *objloc(obj)
   ZIPOBJ obj;
{  /*   Return a pointer (absolute) to the object obj structure.
   OBJLEN and DPTLEN must be changed for EZIP.
   */
   ZIPINT offset;

   offset = (obj - 1) * OBJLEN;   /* offset of object in objtab */
   offset += objtab + DPTLEN;      /* skipping default prop table */
   return(Dataspace + offset);      /* return a real pointer */
}

ZIPINT firstprp(obj)   /* Get 1st property offset for given obj */
   ZIPOBJ obj;
{  /*   This routine is shared by the property operations and returns the
   offset from the start of the Dataspace for a given object's
   properties.  (EZIP.  Modify for new property definitions.)
   */
   ZIPINT offset, len;
   ZIPCHAR *ptr;

   ptr = objloc(obj) + PROPS;      /* get obj pointer (absolute) */
   offset = GTAWRD(ptr);      /* get prop table offset */
   len = (GTVBYT(offset)) << 1;   /* length of short description */
   return(offset + len + 1);      /* skip over short description */
}

ZIPINT nxtprp(propoff)
   ZIPINT propoff;
{  /*   Next property.  This routine take a property offset and wades thru
   the property table to the next property.
   */
   ZIPBYT id, len;

   id = GTVBYT(propoff);      /* get property id */
   len = (id & PSZMSK) >> PROPSIZE;   /* mask, right justify size bits */
   if (len >= 2)
   { /* length follows */
      len = GTVBYT(propoff + 1) & PNUMSK;  /* length in next byte */
   }
   return(propoff + len + 2);      /* skip extra length byte */
}

/* return a pointer to a flag byte in obj */ 
ZIPCHAR *flagptr(obj, flagnum)
   ZIPOBJ obj; 
   ZIPINT *flagnum; 
{  /* Flagptr is a common routine to the three flag operations which returns
      a pointer to the correct flag byte (containing flagnum) for obj.
      Also, flagnum is normalized to a value between 0..7, from the possible
   range of 0..31 (0..47 EZIP).
   */
   ZIPINT offset;
   ZIPCHAR *ptr;

   offset = (*flagnum) >> 3;      /* desired flag byte */
   *flagnum = (*flagnum) & 7;      /* desired bit within byte */
   ptr = objloc(obj);
   return(ptr + offset);      /* return adjusted pointer */
}

/************************************************************************
*                                                                       *
*   V A R I A B L E   A N D   P R E D I C A T E   S U P P O R T         *
*                                                                       *
************************************************************************/

short getarg(mode)
char mode;
{  /*   Getarg is a general routine called by NXTINS to get arguments for
   an opcode.  It is called with the addressing mode as an parameter
   to determine if the argument should be retrieved as from the stack,
   as immediate data (long or short), global variable or local.
   */
   ZIPINT result;

   switch (mode)
   {
   case 0:
      result = nxtwrd();   /* long immediate */
      break;
   case 1:
      result = nxtbyt();   /* short immediate */
      break;
   case 2:
      if ( (result = nxtbyt()) != 0)    /* variable (type detrmd by getvar) */
         result = getvar(result);
      else
         result = POPZ();      /* stack */
      break;
   default:
      fatal("Undefined address mode");
   }
   return result;
}

ZIPINT getvar(var)
   ZIPBYT var;
{  /*    Getvar retrieves a variable value as dictated by var.  0 indicates
   return tos, 1-15 are local variables referenced through zlocs, and
   16-255 are global.
   */
   ZIPINT global, retval;

   if ( var != 0 )
   { /* not a stack variable */
      if ( var >= LOCAL )
      { /* not local, thus global */
         global = ((var - 16) << 1) + glotab;   /* basify, num*2 + offset */
         retval = GTVWRD(global);
      }
      else
      { /* get a local value */
         --var;
         retval = GETLOC(var);
      }
   }
   else
   { /* return value on top of stack (don't pop) */
      retval = *zsp;
   }
   return retval;
}

void putvar(var, value)
   ZIPBYT var;
   short value;
{  /* Sets variable (var) to value.  See above for description of variables */
   ZIPINT global;

   if ( var != 0 )
   { /* not a stack variable */
      if (var >= LOCAL)
      { /* not local, thus global */
         global = ((var - 16) << 1) + glotab;   /* basify, num*2 + offset */
         PTVWRD(global, value);         /* set the variable */
      }
      else
      { /* set a local value */
         --var;
         SETLOC(var, value);
      }
   }
   else
   { /* update top-of-stack (don't push) */
      *zsp = var;
   }
}


void putval(value)
   short value;
{  /*     Many opcodes return a value.  Putval uses an immediate byte of data
   to determine to what location the value is returned.
   */
   ZIPBYT loc;
   if ( (loc = nxtbyt()) != 0 )      /* get location indicator */
      putvar(loc, value);
   else
      PUSHZ(value);
}

void bytval(value)
   ZIPBYT value;
{  /*  Bytval performs a putval but assures that high bits are off. */
   putval(value & 255);
}

void ppred(truth)      /* do a predicate jump */
   ZIPINT truth;
{  /*     Ppred performs a predicate jump based on truth and immediate values.
   An immediate byte is picked up to determine if the jump is long or
   short or if a return true or false should be done.
   */
   ZIPBYT jump1, jump2;    /* predicate jump values */
   short offset;

   if ( truth != 0 )
   { /* truth doesn't have to be 1 */
      truth = 1;
   }

   jump1 = nxtbyt();      /* get jump value */
   if ( (jump1 & BACKWARD) == 0 )
   { /* jump if predicate failed */
      truth++;         /* increment flag */
   }
   if (jump1 & JMPLNTH)
   { /* one byte jump offset */
      offset = jump1 & PREDMSK;   /* mask off special bits */
   }
   else
   { /* nope, one byte jump */
      jump1 &= PREDMSK;          /* clear out special bits */
      jump2 = nxtbyt();          /* get low order byte */
      offset = jump1;            /* get high order bits */
      offset = (jump1 << 8) + jump2;   /* make a word from bytes */
      if (offset & BIT14)        /* is it a 14 bit 2's comp number */
         offset |= COMP16;       /* make into a 16 bit 2's comp */
   }

   if ( truth == 1 )
   { /* jump according to truth */
      if ( offset != 0 )
      { /* do jump if there is an offset */
         if ( offset != 1 )
         { /* do a jump */
            zpc2 += offset - 2;   /* add it to pc, with fixes */
            newzpc();
         }   
         else
         { /* just return a true */
            zret(ZTRUE);
         }
      }
      else
      { /* just return false then */   
         zret(ZFALSE);
      }
   }   
}

void docall( retval )
   ZIPINT retval;
{
   int ix, jx;
   unsigned long temp1;
   unsigned short temp2;

   if ( argblk[1] != 0 )
   {
/* Save both halves of the PC, the appropriate stack pointer to restore,
   the number of arguments for the calling routine, and whether the
   calling routine wants this call to return a value. */

      PUSHZ(zpc1);            /* save return location */
      PUSHZ(zpc2);
      PUSHZ(zlocs);            /* save locals */
      PUSHZ (zargct | (retval << 8));      /* other stuff */

      temp2 = (unsigned short)(argblk[1]);
      temp1 = (unsigned long)((temp2 + (FOFF8/4)));

      bsplitq2(temp1);         /* split new code ptr */
      zargct = (char)argblk[0] - 1;         /* number of args */
      zpc1 = zblk;
      zpc2 = zoff;
      newzpc();            /* update the zpc */
      zlocs = zsp - zstack;         /* make a locals pointer */
      zlocs--;             /* to next stack slot*/
      ix = nxtbyt();            /* get num locals byte */
      argblk[0]--;            /* arg[0] has locs to init */
      jx = 2;               /* index to first opt arg */

      while (ix-- != 0)
      { /* set optional args */
         if (argblk[0] < 1)
         { /* no optional args, so init to zero */
            PUSHZ(0);
         }
         else
         { /* push on the args */
            PUSHZ(argblk[jx]);
            jx++;
            argblk[0]--;         /* dec count of init vals */
         }
      }   
   }
   else if ( retval )
   { /* who knows, if not calling anything */
      putval(ZFALSE);
   }
}

int
internal_call(rout)
   short rout;
{
   int retval = 0;

   if ( rout != 0 )
   { /* Only do this if routine exists */
      PUSHZ(zpc1);      /* Save the PC */
      PUSHZ(zpc2);
      zpc1 = zpc2 = 0;         /* And set to 0, so return will know */
      argblk[0] = 1;      /* Set up fake argblk */
      argblk[1] = rout;
      docall(ZTRUE);      /* Do the call setup */
      if (setjmp(icall_env) == 0) 
         main_loop();      /* Run the main loop */
      retval = POPZ();      /* Return will leave value on stack */
   }
   return retval;
}


/* this will NOT work to return through an internal call (as might
   occur for a timeout routine, or in a CR interrupt).  The horror
   of that is unimaginable. */
void do_throw(rval,frame)
   ZIPINT rval,frame;
{
   if (zlocs > frame) 
      fatal("Bad frame to THROW");

   zlocs = frame;      /* restore top of stack */
   zret(rval);         /* and fall into return code */
}

