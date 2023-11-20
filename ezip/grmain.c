#include "stdio.h"

#define XDISP 20
#define YDISP 0

main() {
    int f, p, savmode;
    int fpic, lastpic, npics;
    savmode = getmode() & 15;
    setmode(4);
    grinit();
    for (f = 2; f <= 4; f++) {
	font(f);
	npics = fontsize(f);
	fpic = fontoff(f);
	lastpic = fpic + npics - 1;
	for (p = fpic; p <= lastpic; p++) {
	    disppict(p, XDISP, YDISP);
	    getchar();			/* wait for user input */
	    clear();
	    }
	}
    setmode(savmode);
}

#define ROWS 200
#define COLS 320

clear() {
    int r,c,attr;
    attr = 0;
    for (r = 0; r < ROWS; r++) {
	for (c = 0; c < COLS; c++) {
	    wdot(r, c, attr);
	    }
	}
}