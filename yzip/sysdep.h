/************************************************************************
*                                                                       *
*                                                                       *
*       S Y S D E P . H                                                 *
*                                                                       *
*                                                                       *
*************************************************************************/
/* constants and such used in the system dependent files, like sysdep
* and photo
*/

#define LOWFONT 3
#define HIGHFONT 6
#define FONT_WIDTH 8
#define MCGA_FONT_WIDTH 5     /* width of zero char in MCGA mode */
#define FONT_HEIGHT 8

#define CURSOR_BLINK 20      		/* 100ths of a second between blinks */
#define DOUBLE_CLICK_COUNT 30 	/* 100ths of a second between clicks */

#define LOWFONT 3
#define HIGHFONT 6

#define MCGADELETE 127

#define EPSILON 3          /* how close to get in asking for memory */
#define PRT_BUFF_LEN 80    /* size of scripting buffer */
#define FONT_HDR_SIZE 34   /* how many bytes in font header */
#define DEF_PIC_BUFF_SIZE 64 	/* default 32Kb picture buffer size */
#define MAX_PB_SIZE 126			/* 63Kb is biggest it can be */
#define HASH_TABLE_SIZE 32		/* 16Kb hash table size */

/* Video screen defines */
#define VID_CGA 0x01
#define VID_EGA 0x02
#define VID_MCGA 0x04
#define VID_TANDY 0x08

/* how wide are the 4 major modes, in pixels */
#define MCGA_WIDTH 320
#define EGA_WIDTH 640
#define CGA_WIDTH 640
#define TANDY_WIDTH 640

/* height is always 200 */
#define SCREEN_HEIGHT 200              /* height of video screen */

/* some defines for our special int86 and int86x */
#define CS_REG 1              /* get/set CS: */
#define DS_REG 2              /* get/set DS: */
#define ES_REG 3              /* get/set ES: */

/* get me the character width macro */
#define CWIDTH( CH ) (\
   (((CH) >= SPACE) && ((CH) <= CURSOR_CHAR)) ? \
	( (Win_hlmode & MONO_SPACE) ? Zip_font_width : \
    ((Display&VID_MCGA) ? FontWid[(CH)-0x20]: FONT_WIDTH)) : \
    (0))

#define CURSOR_CHAR ('~'+1)    /* Special Cursor char in MCGA character set */

#define DEF_MCGA_FG_COLOR 0x1F   /* bright white */
#define DEF_MCGA_BG_COLOR 0x18   /* gray */
#define MCGA_EXT ".mg1"				/* extension for MCGA picture file */

#define DEF_EGA_FG_COLOR 0x0F    
#define DEF_EGA_BG_COLOR 0x01
#define EGA_EXT ".eg1"				/* extension for EGA picture file */

#define DEF_CGA_FG_COLOR 0x01    /* white */
#define DEF_CGA_BG_COLOR 0x00    /* black */
#define CGA_EXT ".cg1"				/* extension for CGA picture file */

#define DEF_TANDY_FG_COLOR 0x01
#define DEF_TANDY_BG_COLOR 0x00

/*** Mouse Definitions ***/

/** Defined if using mouse flag, rather than check 
#define MAN_MOUSE 1	
**/

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
#define FIND_MOUSE 13
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

/*
#define DO_EGA 1*/	/* if defined, then round for EGA/CGA positions */
