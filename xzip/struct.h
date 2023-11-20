#ifndef ZIPDINC
#include "zipdefs.h"
#endif

/* EZIP should include ezipdefs.h */


/************************************************************************
*									*
*	S T R U C T U R E S						*
*									*
************************************************************************/

struct devhead {
	long	dhpnext;
	int	dhattrs;
	int	dhstrat;
	int	dhint;
	char	dhname[8]; };

struct blkdesc {
    struct blkdesc *next,	/* next descriptor ptr */
		   *prev;	/* previous descriptor ptr  */
    ZIPCHAR *loc;		/* page pointer */
    short vpage;		/* page number */
    };

/*  D E B U G G I N G  */

#if _DEBUG

struct ops {
    char brkflg,
         *opstr;
    };

struct history_list {
   short z1, z2,
         argblk[MAXARGS],
         opcode;
   char *opstring;
};

#endif
