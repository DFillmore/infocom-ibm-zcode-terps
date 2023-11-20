/* ibmp() ibm predicate -- test for the machine type */
#include "dos.h"	/* definition for FP_SEG */
#define MAXSCAN 22
char *copyright[] = {
    "COPR. IBM",	/* 1 returned */
    "COMPAQ Co",	/* 2 */
    "Tandy Cor",	/* 3 */
    0			/* end of table */
    };

ibmp() {
    char *testcp, chr, *rtcp, first;
    char far *bioscp, far *rbcp;
    int i = 1;			/* return 1-origin index of string */
    char **cpp = &copyright[0];	/* point at first element in table */
    int scan;
    while (testcp = *cpp++) {	/* get first test string, null=>no more */
	FP_SEG(bioscp) = 0xF000;	/* segment of copyright notice */
	FP_OFF(bioscp) = 0xE000;	/* offset in segment */
	scan = MAXSCAN;			/* number of chrs to search */
	first = *testcp++;
	while (--scan) {
	    if (first == *bioscp++) {	/* find first chr */
		rtcp = testcp;	/* compare rest */
		rbcp = bioscp;
		while ((chr = *rtcp++)	/* end of test string? */
		    && chr == *rbcp++)	/* match in bios? */
		    ;
		if (chr == 0) return(i);	/* reached end w/out err ? */
		}			/* end if -- first chr */
	    }				/* end while(--scan) */
	i++;
	}
    return(0);
}
	
main() {
    switch(ibmp()) {
	case 1: printf("It's a genuine IBM\n"); break;
	case 2: printf("It's a genuine COMPAQ\n"); break;
	case 3: printf("It's a genuine TANDY\n"); break;
	default: printf("It's a genuine InCoMpAtIbLe\n"); break;
	}
}