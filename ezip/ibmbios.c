#include <dos.h>

/* By Daniel Easley
   (ehb) Dan had commented out his set cursor pos and get cursor pos
   code for some reason */

struct WINDOW
	{
	char left;
	char top;
	char right;
	char bottom;
	};

/**
 *	SCROLL - Scroll a screen region
 **/

bios_scroll(window, number, attribute)
struct WINDOW *window;
int number, attribute;
	{
	if (number)
		{
		union REGS regs;

		regs.x.cx = *(short *) &window->left;
		regs.x.dx = *(short *) &window->right;
		regs.h.bh = attribute;

		if (number > 0)
			{
			regs.h.al = number;
			regs.h.ah = 6;
			}
		else
			{
			regs.h.al = -number;
			regs.h.ah = 7;
			}

		if (regs.h.al >= (window->bottom - window->top + 1))
			regs.h.al = 0;

		int86(0x10, &regs, &regs);
		}
	}

/**
 *	CLRWINDOW - Clear window region
 **/

clrwindow(top, left, bottom, right, attribute)
int top, left, bottom, right, attribute;
	{
	union REGS regs;

	regs.h.ch = top;
	regs.h.cl = left;
	regs.h.dh = bottom;
	regs.h.dl = right;
	regs.h.bh = attribute;
	regs.x.ax = 0x0600;
	int86(0x10, &regs, &regs);
	}


/**
 *	SETCPOS - Set cursor position
 **/

setcpos(line, column)
int line, column;
	{
	union REGS regs;

	regs.h.dh = line;
	regs.h.dl = column;
	regs.h.bh = 0x00;
	regs.h.ah = 0x02;
	int86(0x10, &regs, &regs);
	}

/**
 *	CURCPOS - Set cursor position
 **/

curcpos(line, column)
int *line, *column;
	{
	union REGS regs;

	regs.h.bh = 0x00;
	regs.h.ah = 0x03;
	int86(0x10, &regs, &regs);

	*line	= regs.h.dh;
	*column	= regs.h.dl;
	}


/**
 *	WRITEC - Write character(s)
 **/

writec(c, n)
char c;
int n;
	{
	union REGS regs;
	int line, column;

	curcpos(&line, &column);

	regs.x.cx = (((column + n) >= 80) ? (80 - column) : n);
	regs.h.bh = 0;
	regs.h.al = c;
	regs.h.ah = 0x0a;
	int86(0x10, &regs, &regs);

	setcpos(line, (column + n));
	}

/**
 *	WRITECA - Write character(s) and attribute(s)
 **/

writeca(c, a, n)
char c;
int a, n;
	{
	union REGS regs;
	int line, column;

	curcpos(&line, &column);

	regs.x.cx = (((column + n) >= 80) ? (80 - column) : n);
	regs.h.bh = 0;
	regs.h.bl = a;
	regs.h.al = c;
	regs.h.ah = 0x09;
	int86(0x10, &regs, &regs);

	setcpos(line, (column + n));
	}

/**
 *	WRITES - Write string
 **/

writes(s)
char *s;
	{
	union REGS inregs, outregs;
	int line, column;

	inregs.x.cx = 0x01;
	inregs.h.bh = 0x00;
	inregs.h.ah = 0x0a;

	curcpos(&line, &column);

	while (*s)
		{
		inregs.h.al = *s++;
		int86(0x10, &inregs, &outregs);

		if (column < 79)
			setcpos(line, ++column);
		else
			break;
		}
	}

/**
 *	WRITESA - Write string with attribute
 **/

writesa(s, a)
char *s;
int a;
	{
	union REGS inregs, outregs;
	int line, column;

	inregs.x.cx = 1;
	inregs.h.bh = 0;
	inregs.h.bl = a;
	inregs.h.ah = 9;

	curcpos(&line, &column);

	while (*s)
		{
		inregs.h.al = *s++;
		int86(0x10, &inregs, &outregs);

		if (column < 79)
			setcpos(line, ++column);
		else
			break;
		}
	}

/**
 *	READCA - Read character and attribute
 **/

readca()
	{
	union REGS regs;

	regs.h.bh = 0;
	regs.h.ah = 8;
	int86(0x10, &regs, &regs);

	return(regs.x.ax);
	}

/**
 *	SETSMODE - Set screen mode
 **/

setsmode(mode)
int mode;
	{
	union REGS regs;

	regs.h.al = mode;
	regs.h.ah = 0x00;
	int86(0x10, &regs, &regs);
	}

/**
 *	CURSMODE - Current screen mode
 **/

cursmode()
	{
	union REGS regs;

	regs.h.ah = 0x0f;
	int86(0x10, &regs, &regs);

	return(regs.x.ax);
	}

/**
 *	SETPALETTE - Set color palette
 **/

setpalette(color, id)
int color, id;
	{
	union REGS regs;

	regs.h.bh = color;	
	regs.h.bl = id;
	regs.h.ah = 0x0b;
	int86(0x10, &regs, &regs);
	}

/**
 *	GETKEY - Get next key
 **/
/*
getkey()
	{
	union REGS regs;

	regs.h.ah = 0;
	int86(0x16, &regs, &regs);

	return(regs.x.ax);
	}
*/

/**
 *	KEYREADY - Get key ready status
 **/
/*

keyready()
	{
	union REGS regs;

	regs.h.ah = 1;
	return((int86(0x16, &regs, &regs) & 0x40) ? 0 : regs.x.ax);
	}
*/

/**
 *	KEYSTATE - Get keyboard status flags
 **/

keystate()
	{
	union REGS regs;

	regs.h.ah = 2;
	int86(0x16, &regs, &regs);

	return((int) regs.h.al);
	}
