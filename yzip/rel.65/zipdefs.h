/************************************************************************
*																								*
*	H E A D E R   F I L E   F O R   C   Z I P										*
*																								*
*************************************************************************/
/* The following definitions control whether ZIP,EZIP or XZIP is compiled
   the appropriate definition is made on the command line using the
   -D switch, e.g., -DEZIP */

#ifdef YZIP
/* for now, we will use XZIP and YZIP together */
#define XZIP
#endif

#define ZIPDINC

/* EXZIP is defined for EZIP and XZIP (and of course YZIP now) */
#ifdef YZIP
#define EXZIP 0
#endif

/* what the MODE bits mean */
#define MODE_COLOR 0x01
#define MODE_DISPL 0x02
#define MODE_BOLD 0x04
#define MODE_UNDE 0x08
#define MODE_MONO 0x10
#define MODE_SOUND 0x20

/* The following definition controls inclusion of debugging code */
/*#define _DEBUG  non zero to activate */

/* The following definitions control machine dependent compilation */
typedef unsigned short int ZIPINT;	   /* FOR VIRTUAL ADDRESSES */
#define ZIPCHAR char huge		         /* FOR VIRTUAL ADDRESSES */
typedef unsigned short ZIPOBJ;         /* E/X/Y ZIP need two bytes */
typedef unsigned char ZIPBYT;		      /* for general unsigned bytes */
typedef unsigned char BOOLEAN;         /* for flags */
#define MACHID '\2'			            /* Machine ID = 2 */

typedef unsigned long int ZIPLONG;	/* for yzip's games > 256K see bsplitqy */

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
#define PSERNM 18		/* 6 BYTE SERIAL NUMBER (DATE) */
#define PWRDTB 24		/* POINTER TO FWORD TABLE FOR PUTSTR */
#define PLENTH 26		/* LENGTH OF GAME IN WORDS(ZIP) QUADDS(EXZIP) */
#define MIN_HEADER 52		/* how much we have to read in to get length */
#define PCHKSM 28		/* CHECKSUM OF ALL BYTES, EXCLUDING HEADER */

#define PINTWD 30		/* INTERPRETER ID/VERSION */
#define PSCRWD 32		/* SCREEN SIZE, ROWS/COLS */
#define PHWRD 34		/* screen width, pixels */
#define PVWRD 36		/* screen height, pixels */
#define PFWRD 38		/* font height/font width */
#define PFOFF 40		/* function table offset */
#define PSOFF 42		/* string table offset */

#define PCLRWRD 44		/* background/foreground */
#define PTCHARS 46		/* pointer to table of terminators */
#define PTWIDTH 48      /* running width for table output */
#define PCRFUNC 50		/* function for carriage returns */
#define PCHRSET 52		/* character set table */
#define PEXTAB 54		/* pointer to mouse stuff etc. */
/*extension table offsets  (in words )*/
#define PMSLOCX 1               /* x location of mouse */
#define PMSLOCY 2 		/* y location of mouse */

#define HDRSIZ 64		/* LENGTH OF HEADER IN BYTES */

/* some of the flags meanings */
#define FSCRI 0x01   /* currently scripting */
#define FFIXE 0x02   /* need mono font */
#define FSTAT 0x04	/* status line update */
#define FDISP 0x08	/* flag meaning program needs graphics */
#define FUNDO 0x10   /* flag meaning program wants undo/isave */
#define FMOUS 0x20   /* uses mouse */
#define FCOLO 0x40   /* uses color */
#define FMENU 0x80   /* uses menus */

/*****  GENERAL DEFINITIONS  *****/
#define ZMVERS 6		/* Z-MACHINE VERSION NUMBER */
#define LSTACK 1024		/* VIRTUAL STACK SIZE  [in words (?)] */

#define STKLEN 64		/* SYSTEM STACK SIZE [in longs (?)] */
#define BLKSIZ 512

		/* SIZE OF VIRTUAL PAGES */
#define MAXBLKS 512		/* EXZIP max size */
#define MINMEM 128		/* let's get serious here */
#define KTOBLKS 2		/* K * KTOBLKS -> BLKS */

#define ZTRUE 1
#define ZFALSE 0
#define MAXARGS 9		/* ZIL MAXIMUM ARGS (eight plus count) */

#define SCRIPTBIT 1
#define STATBIT 16
#define SPLTBIT 32

/***** SOUNDS md_sound() CAN MAKE *****/
#define BEEP 1
#define BOOP 2
#define FEEP 3

/*****  CHAR INPUT AND STRING OUTPUT  *****/

#define NO_INPUT -1
#define Z_EOL 1
#define EOL 10			      /* LINE FEED IS THE UNIX EOL */
#define ESC 27

#define BKSPC 8			   /* BACKSPACE */
#define PADCHR 5		      /* PAD CHAR FOR ZSTRINGS */

#define SPACE '\x20'		   /* INITIAL READ BREAD CHARS */
#define TAB '\x09'
#define CR '\x0D'
#define FF '\x0C'
#define EOS '\x0B'         /* End of sentence character (^K) */
#define PERIOD '.'
#define COMMA ','
#define QMARK '?'

/* special keys and their numbers */
#define UP_ARROW 129
#define DOWN_ARROW 130
#define LEFT_ARROW 131
#define RIGHT_ARROW 132
#define F1 133
#define F2 134
#define F3 135
#define F4 136
#define F5 137
#define F6 138
#define F7 139
#define F8 140
#define F9 141
#define F10 142
#define F11 143
#define F12 144

#define KEY0 145
#define KEY1 146
#define KEY2 147
#define KEY3 148
#define KEY4 149
#define KEY5 150
#define KEY6 151
#define KEY7 152
#define KEY8 153
#define KEY9 154

#define DOUBLE_CLICK 253
#define SINGLE_CLICK 254

#define CHRS_PER_ZWORD 9

#define TOKEN_TBL_LEN 59
#define BYTEMODE 0
#define ZCHRLEN 5
#define ZCHRMSK 31
#define CSETLEN 26

#define NORMAL 0
#define REVERSE 1

#define BOLD 2
#define UNDER_LINE 4
#define MONO_SPACE 8

/*****  BUFFERING EQUATES  *****/
#define IBUFSIZ 100
#define OBUFSIZ 100

#define CONSOLE 0
#define PIPE 1
#define PBUFSIZ 85
#define PIPEWIDTH 79

/*****  OBJECT AND PROPERTY DEFINITIONS  *****/
#define FLAGS1 0		/* OFFSET TO FLAG WORDS */
#define FLAGS2 2

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
#define NOT_IN_CORE -2		/* a NULL pointer */
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
#define PRED(arg) (ppred(arg))
#define PUTV(arg) (putval(arg))
#define OBJVAL(obj) (putval(obj))
#define PUSHZ(arg) (*--zsp = (ZIPINT)arg)
#define POPZ() (*zsp++)

#define BYTARG(arg) ((ZIPBYT)(argblk[arg]))
#define OBJARG(arg) (argblk[arg])
#define GETOID(ptr)  (GTAWRD(ptr))
#define PUTOID(ptr, val)  (PTAWRD(ptr, val))


#define GETLOC(var) (*(zstack + (zlocs - var)))
#define SETLOC(var, val) (*(zstack + (zlocs - var)) = val) 

#define GTABYT(ptr) ((*(ptr)) & BYTEMSK)	/* ptr may be a compound */
#define GTVBYT(off) ((*(Dataspace + off)) & BYTEMSK)

#define PTABYT(ptr,value) (*(ptr) = (char)value)
#define PTVBYT(off,value) (*(Dataspace+off) = (char)value)

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

#define OPCATCH 185		/* xzip only */
#define OPQUIT 186
#define OPCRLF 187
#define OPUSL 188
#define OPVERI 189
#define OPEXTOP 190

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
#define OPICALL1 143
#define OPCALL1 136

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

#define OPCALL2 25
#define OPICALL2 26
#define OPCOLOR 27
#define OPTHROW 28
#define LAST_TWO_OP OPTHROW
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
#define OPWINPOS 272
#define OPWINSIZE 273
#define OPWINATTR 274
#define OPWINGET 275
#define OPSCROLL 276
#define OPFSTACK 277
#define OPMSEINFO 278
#define OPMSELIM 279
#define OPXPUSH 280
#define OPWINPUT 281
#define OPPRINTF 282
#define OPMENU	283
#define OPPICSET 284

/* Window Defines */
#define NUMATTR 15 /* # of window attributes avail. to games writer */
#define WRAPMASK 0x01
#define SCROLLMASK 0x02
#define SCRIPTMASK 0x04
#define BUFFERMASK 0x08

/* Video Defines */
/* As passed to COLOR, but zero based, starting with black */
#define ZIP_BLACK 2
#define ZIP_RED 3
#define ZIP_GREEN 4
#define ZIP_YELLOW 5
#define ZIP_BLUE 6
#define ZIP_MAGENTA 7
#define ZIP_CYAN 8
#define ZIP_WHITE 9
#define ZIP_GREY 10
#define ZIP_BROWN 11



