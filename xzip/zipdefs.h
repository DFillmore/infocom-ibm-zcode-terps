/***********************************************************************
*									*
*	H E A D E R   F I L E   F O R   C   Z I P			*
*									*
************************************************************************/
/* The following definitions control whether ZIP,EZIP or XZIP is compiled
   the appropriate definition is made on the command line using the
   -D switch, e.g., -DEZIP */

#ifndef XZIP
#define XZIP
#endif

#define ZIPDINC

/* #define ZIP 0 */
/* #define EZIP 0 */
/* #define XZIP 0 */

/* EXZIP is defined for EZIP and XZIP */

#ifdef EZIP
#define EXZIP 0
#endif

#ifdef XZIP
#define EXZIP 0
#endif

#define EMODE 0x3f    /* inverse/bold/underline/split/sound */
#define XMODE 0x3c      /* mono/underline/bold/sound/no color */
#define EHUND 8
#define XCOLOR 1
#define XUNDE 8

/* The following definition controls inclusion of debugging code */
#define _DEBUG 0	/* non zero to activate */

/* The following definitions control machine dependent compilation */

#define IBMPC 0

/* #define ATT7300 0 */
#ifdef ATT7300
#define SYSTEM5 0
#endif

#ifdef SYSTEM5
typedef unsigned short int ZIPINT;	/* FOR VIRTUAL ADDRESSES */
#define ZIPCHAR char			/* FOR VIRTUAL ADDRESSES */
     #ifdef ZIP
     typedef unsigned char ZIPOBJ;	/* FOR USE WITH OBJECTS  (ZIP) */
     #endif
     #ifdef EXZIP
     typedef unsigned short ZIPOBJ;     /* E and X ZIP need ZIPOBJ
                                           to be two bytes long */
     #endif
typedef unsigned char ZIPBYT;		/* for general unsigned bytes */
#define MACHID '\143'			/* Machine ID = 99 decimal */
#endif

#ifdef IBMPC
typedef unsigned short int ZIPINT;	/* FOR VIRTUAL ADDRESSES */
#define ZIPCHAR char huge		/* FOR VIRTUAL ADDRESSES */
     #ifdef ZIP
     typedef unsigned char ZIPOBJ;	/* FOR USE WITH OBJECTS  (ZIP) */
     #endif
     #ifdef EXZIP
     typedef unsigned short ZIPOBJ;     /* E and X ZIP need two bytes */
     #endif
typedef unsigned char ZIPBYT;		/* for general unsigned bytes */
#define MACHID '\2'			/* Machine ID = 2 */
#endif

/*****  HEADER INFORMATION OFFSETS  *****/

#define PVERS1 0		/* ZVERSION BYTE */
#define PVERS2 1		/* ZVERSION MODE BYTE */
#define PZRKID 2		/* ZORK ID */
#define PENDLD 4		/* ENDLOD POINTER */
#define PSTART 6		/* Z ENTRY POINT */
#define PVOCTB 8		/* POINTER TO VOCABULARY TABLE */
#define POBJTB 10		/* POINTER TO OBJECT TABLE */
#define PGLOTB 12		/* POINTER TO GLOBAL TABLE */
#define PPURBT 14		/* POINTER TO BEGINNING OF PURE CODE */
#define PFLAGS 16		/* POINTER TO FLAGS */
#define FSTAT 4			/* status line update */
#define FDISPL 8		/* flag meaning program needs graphics */
#define FUNDO 16		/* flag meaning program wants undo/isave */
#define PSERNM 18		/* 6 BYTE SERIAL NUMBER (DATE) */
#define PWRDTB 24		/* POINTER TO FWORD TABLE FOR PUTSTR */
#define PLENTH 26		/* LENGTH OF GAME IN WORDS(ZIP) QUADDS(EXZIP) */
#define MIN_HEADER 52		/* how much we have to read in to get length */
#define PCHKSM 28		/* CHECKSUM OF ALL BYTES, EXCLUDING HEADER */

#define PINTWD 30		/* INTERPRETER ID/VERSION */
#define PSCRWD 32		/* SCREEN SIZE, ROWS/COLS */

#ifdef XZIP
/* XZIP words */
#define PHWRD 34		/* screen width, pixels */
#define PVWRD 36		/* screen height, pixels */
#define PFWRD 38		/* font height/font width */
#define PLMRG 40		/* left margin, pixels */
#define PRMRG 42		/* right margin, pixels */
#define PCLRWRD 44		/* background/foreground */
#define PTCHARS 46		/* pointer to table of terminators */
#define PCRCNT 48		/* counter for carriage returns */
#define PCRFUNC 50		/* function for carriage returns */
#define PCHRSET 52		/* character set table */
#define PEXTAB 54		/* pointer to mouse stuff etc. */
/*extension table offsets  (in words )*/
#define PMSLOCX 1               /* x location of mouse */
#define PMSLOCY 2 		/* y location of mouse */
#endif

#define HDRSIZ 64		/* LENGTH OF HEADER IN BYTES */

/*****  GENERAL DEFINITIONS  *****/

#ifdef ZIP
#define ZMVERS 3		/* Z-MACHINE VERSION NUMBER */
#define LSTACK 512		/* VIRTUAL STACK SIZE  [in words (?)] */
#endif
#ifdef EZIP
#define ZMVERS 4		/* Z-MACHINE VERSION NUMBER */
#define LSTACK 1024		/* VIRTUAL STACK SIZE  [in words (?)] */
#endif
#ifdef XZIP
#define ZMVERS 5		/* Z-MACHINE VERSION NUMBER */
#define LSTACK 1024		/* VIRTUAL STACK SIZE  [in words (?)] */
#endif

#define STKLEN 64		/* SYSTEM STACK SIZE [in longs (?)] */

#define BLKSIZ 512
		/* SIZE OF VIRTUAL PAGES */
#ifdef ZIP
#define MAXBLKS 256		/* MAXIMUM GAME SIZE (EZIP is 512) */
#define MINMEM 48		/* MINMUM MEMORY ALLOWED */
#else
#define MAXBLKS 512		/* EXZIP max size */
#define MINMEM 128		/* let's get serious here */
#endif
#define KTOBLKS 2		/* K * KTOBLKS -> BLKS */

#define ZTRUE 1
#define ZFALSE 0
#ifdef ZIP
#define MAXARGS 5		/* ZIL MAXIMUM ARGS (four plus count) */
#endif
#ifdef EXZIP
#define MAXARGS 9		/* ZIL MAXIMUM ARGS (eight plus count) */
#endif

#define SCRIPTBIT 1
#define STATBIT 16
#define SPLTBIT 32

#define DEF_FG_COLOR 7		/* white */
#define DEF_BG_COLOR 1		/* blue */


#define THEN			/* if <cond> THEN <exp>; else <exp>; */
#define NULL 0

#define G_HERE 16		/* ZIL status-line globals */
#define G_SCORE 17
#define G_MOVES 18
#define G_HOURS 17
#define G_MINS 18

/* for use in calls to md_mouse() */
#ifdef XZIP
#define INIT_MOUSE 0
#define SHOW_MOUSE 1
#define HIDE_MOUSE 2
#define GET_MOUSE_POSITION 3
#define SET_MOUSE_POSITION 4
#define GET_MOUSE_PRESS 5
#define GET_MOUSE_RELEASE 6
#define SET_MOUSE_X_BOUNDS 7
#define SET_MOUSE_Y_BOUNDS 8
#define SET_MOUSE_GRAPHICS_BLOCK 9
#define SET_MOUSE_TEXT_CURSOR 10
#define READ_MOUSE_MOTION 11
#define SET_MOUSE_INPUT_MASK 12
/* MOUSE_MASK is for SET_MOUSE_INPUT_MASK    bit    condition              */
/*                                            0     cursor position change */
/*					      1     left button pressed    */
/*					      2    left button released    */
/*					      3     right button pressed   */
/*					      4     right button released  */
#define MOUSE_MASK 0x1f
#define MOUSE_LIGHT_PEN_ON 13
#define MOUSE_LIGHT_PEN_OFF 14
#define SET_MOUSE_PIXEL_RATIO 15
#define MOUSE_CONDITIONAL_OFF 16
#define SET_MOUSE_DOUBLE_SPEED 19
#define MOUSE_EVENT_LENGTH 4     /* size of ring buffer for mouse events */
#endif
/***** SOUNDS md_sound() CAN MAKE *****/
#define BEEP 1
#define BOOP 2
#define FEEP 3

/*****  CHAR INPUT AND STRING OUTPUT  *****/

#define NO_INPUT -1
#define Z_EOL 1
#define EOL 10			/* LINE FEED IS THE UNIX EOL */
#define ESC 27

#if 0 
#define FEEP "\007"		/* BELL */
#endif

#define BKSPC 8			/* BACKSPACE */
#define PADCHR 5		/* PAD CHAR FOR ZSTRINGS */
#define SPACE '\040'		/* INITIAL READ BREAD CHARS */
#define TAB '\011'
#define CR '\015'
#define FF '\014'
#define PERIOD '.'
#define COMMA ','
#define QMARK '?'

#ifdef ZIP
#define CHRS_PER_ZWORD 6
#endif
#ifdef EXZIP
#define CHRS_PER_ZWORD 9
#endif

#define TOKEN_TBL_LEN 59
#define BYTEMODE 0
#define ZCHRLEN 5
#define ZCHRMSK 31
#define CSETLEN 26

#define NORMAL 0
#define REVERSE 1

#ifdef EXZIP
#define BOLD 2
#define UNDER_LINE 4
#define MONO_SPACE 8
#endif

/*****  BUFFERING EQUATES  *****/

#define IBUFSIZ 100
#define OBUFSIZ 100

#define CONSOLE 0
#define PIPE 1
#define PBUFSIZ 85
#define PIPEWIDTH 79

#define SCREEN_LENGTH 24

#define STATLEN 1		/* STATUS LINE ROWS */

/*****  OBJECT AND PROPERTY DEFINITIONS  *****/

#define FLAGS1 0		/* OFFSET TO FLAG WORDS */
#define FLAGS2 2
#ifdef ZIP
#define PARENT 4		/* PARENT OBJECT NUMBER */
#define SIBLING 5		/* SIBLING OJECT NUMBER */
#define CHILD 6			/* CHILD OBJECT NUMBER */
#define PROPS 7			/* POINTER TO PROPERTY TABLE */

#define OBJLEN 9		/* LENGTH OF AN OBJECT */
#define DPTLEN (31 * 2)		/* DEFAULT PROP TABLE LENGTH, 31 ENTRIES */

#define PNUMSK 0x1F		/* PROPERTY ID NUMBER MASK (LOW 5 BITS) */
#define PSZMSK 0xE0		/* PROPERTY SIZE MASK (HIGH 3 BITS) */
#define PROPSIZE 5		/* SHIFT COUNT TO ISOLATE SIZE BITS */
#endif

#ifndef ZIP		/* *** EZIP or CZIP *** */
#define FLAGS3 4		/* EZIP has one more flag word */
#define PARENT 6		/* PARENT OBJECT NUMBER */
#define SIBLING 8		/* SIBLING OJECT NUMBER */
#define CHILD 10		/* CHILD OBJECT NUMBER */
#define PROPS 12		/* POINTER TO PROPERTY TABLE */

#define OBJLEN 14		/* LENGTH OF AN OBJECT */
#define DPTLEN (63 * 2)		/* DEFAULT PROP TABLE LENGTH, 63 ENTRIES */

#define PNUMSK 0x3F		/* PROPERTY ID NUMBER MASK (LOW 6 BITS) */
#define PSZMSK 0xC0		/* PROPERTY SIZE MASK (HIGH 2 BITS) */
#define PROPSIZE 6		/* SHIFT COUNT TO ISOLATE SIZE BITS */
#endif

/*****  PREDICATE AND VARIABLE DEFINITIONS  *****/

#define LOCAL 16
#define EMPTY 0
#define BYTEMSK 255
#define BIT8 0x80
#define BIT16 0x8000
#define BACKWARD 128	/* bits to test */
#define JMPLNTH 64
#define PREDMSK 63
#define BIT14 0x2000	/* for testing 14 bit two's-complement */
#define COMP16 0xC000	/* for converting above to 16 bit */

/*****  PAGING DEFINITIONS  *****/

#define NOT_IN_CORE 0		/* a NULL pointer */
#define NO_PAGE -1		/* avoid conflict with page 0 */
#define BLOCKBITS 0x7F
#define BYTEBITS 0x1FF
#define CVTBLK 9
#define TO_K 10

/*****  FILE I/O DEFINITIONS  *****/

#define PATHSIZ 64
#define RDONLY 0
#define FMODE 0644		/* NEW FILE, set most bits */

/******  DEBUGGING DEFINITIONS  ******/

#define OFF 0
#define ON 1
#define STEP 2
#define SKIPC 4
#define SKIPN 8
#define VERBOSE 16
#define BRKPT 32
#define SKIPS 0xF3

#define HEX 'x'
#define DEC 'd'

/*****  MACROS  *****/

/* #define PRED(arg) ((arg) ? ppred(ZTRUE) : ppred(ZFALSE)) */
#define PRED(arg) (ppred(arg))
#define PUTV(arg) (putval(arg))
#define OBJVAL(obj) (putval(obj))        /* should this be EZIP specific ?
                                            I think not - ASK */

#define PUSH(arg) (*--ssp = arg)
#define POP() (*ssp++)
#define PUSHZ(arg) (*--zsp = arg)
#define POPZ() (*zsp++)

#define BYTARG(arg) ((ZIPBYT)(argblk[arg]))

/* an object ID is a byte in ZIP and a WORD in EZIP */
#ifdef ZIP
#define OBJARG(arg) ((ZIPBYT)(argblk[arg]))
#define GETOID(ptr)  (*ptr)
#define PUTOID(ptr, val)  (*ptr = val)
#endif
#ifdef EXZIP
#define OBJARG(arg) (argblk[arg])
#define GETOID(ptr)  (GTAWRD(ptr))
#define PUTOID(ptr, val)  (PTAWRD(ptr, val))
#endif

#define GETLOC(var) (*(zstack + (zlocs - var)))
#define SETLOC(var, val) (*(zstack + (zlocs - var)) = val) 

#define GTABYT(ptr) ((*(ptr)) & BYTEMSK)	/* ptr may be a compound */
#define GTVBYT(off) ((*(dataspace + off)) & BYTEMSK)

#define PTABYT(ptr,value) (*(ptr) = value)
#define PTVBYT(off,value) (*(dataspace+off) = value)

/*	MOVED INTO PROGRAM SUBROUTINES:

#define GTAWRD(ptr) ((((ZIPBYT) *ptr) << 8) | (ZIPBYT) *(ptr+1))
#define GTVWRD(off) ((((ZIPBYT) *(dataspace+off)) << 8) | (ZIPBYT) *(dataspace+off+1))

#define PTAWRD(ptr,value) ((*ptr = (value >> 8)), (*(ptr+1) = (value & 255))) 
#define PTVWRD(off,value) ((*(dataspace+off) = (value >> 8)), (*(dataspace+off+1) = (value & 255))) 
*/

/*****  OPCODES  *****/

#define ONE_OP 128
#define ZERO_OP 176
#define EXT_OP 192
#define ZEROMSK 15
#define ONEMSK 15
#define TWOMSK 31
#define EXTMSK 63
#define TWOMOD1 64
#define TWOMOD2 32
#define ONEMODE 48

/* ZERO OPS */

#define OPRTRU 176		/* RETURN TRUE */
#define OPRFAL 177		/* RETURN FALSE */
#define OPPRNI 178		/* PRINTI */
#define OPPRNR 179
#define OPNOOP 180
#define OPSAVE 181
#define OPREST 182
#define OPRSTT 183
#define OPRSTA 184

#ifndef XZIP
#define OPFSTA 185		/* flushed in xzip...*/
#endif
#ifdef XZIP
#define OPCATCH 185		/* xzip only */
#endif
#define OPQUIT 186
#define OPCRLF 187
#define OPUSL 188
#define OPVERI 189
#ifdef XZIP
#define OPEXTOP 190
#endif

/* ONE OPS */

#define OPQZER 128
#define OPQNEX 129
#define OPQFIR 130
#define OPLOC 131
#define OPPTSI 132
#define OPINC 133
#define OPDEC 134
#define OPPRNB 135
#define OPREMO 137
#define OPPRND 138
#define OPRETU 139
#define OPJUMP 140
#define OPPRIN 141
#define OPVALU 142
#ifndef XZIP
#define OPBCOM 143
#endif
#ifdef XZIP
#define OPICALL1 143
#endif
#ifdef EXZIP
#define OPCALL1 136
#endif

/* TWO OPS AND EXTENDED OPS */

#define OPUNDF 0
#define OPQEQU 1
#define OPQLES 2
#define OPQGRT 3
#define OPQDLE 4
#define OPQIGR 5
#define OPQIN 6
#define OPBTST 7
#define OPBOR 8
#define OPBAND 9
#define OPQFSE 10
#define OPFSET 11
#define OPFCLE 12
#define OPSET 13
#define OPMOVE 14
#define OPGET 15
#define OPGETB 16
#define OPGETP 17
#define OPGTPT 18
#define OPNEXT 19
#define OPADD 20
#define OPSUB 21
#define OPMUL 22
#define OPDIV 23
#define OPMOD 24

#ifdef EXZIP
#define OPCALL2 25
#endif
#ifdef XZIP
#define OPICALL2 26
#define OPCOLOR 27
#define OPTHROW 28
#endif
#ifdef ZIP
#define LAST_TWO_OP OPMOD
#endif
#ifdef EZIP
#define LAST_TWO_OP OPCALL2
#endif
#ifdef XZIP
#define LAST_TWO_OP OPTHROW
#endif
#define XQEQU 193
#define OPCALL 224
#define OPPUT 225
#define OPPUTB 226
#define OPPUTP 227
#define OPREAD 228
#define OPPRNC 229
#define OPPRNN 230
#define OPRAND 231
#define OPPUSH 232
#define OPPOP 233
#define OPSPLT 234
#define OPSCRN 235

#ifdef EXZIP
#define OPXCALL 236
#define OPCLEAR 237
#define OPERASE 238
#define OPCURSET 239
#define OPCURGET 240
#define OPHLIGHT 241
#define OPBUFOUT 242
#define OPDIROUT 243
#define OPDIRIN 244
#define OPSOUND 245
#define OPINPUT 246
#define OPINTBL 247
#endif
#ifdef XZIP
#define OPBCOM 248
#define OPICALL 249
#define OPIXCALL 250
#define OPLEX 251
#define OPZWSTR 252
#define OPCOPYT 253
#define OPPRINTT 254
#define OPASSNQ 255
#define OPXSAVE 256
#define OPXRESTORE 257
#define OPSHIFT 258
#define OPASHIFT 259
#define OPFONT 260
#define OPDISPLAY 261
#define OPPICINF 262
#define OPDCLEAR 263
#define OPMARGIN 264
#define OPISAVE 265
#define OPIRESTORE 266
#endif
