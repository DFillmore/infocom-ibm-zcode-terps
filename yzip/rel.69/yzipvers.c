#include <dos.h>

#include "zipdefs.h"
#include "struct.h"
#include "extern.h"

#define INTID ZMVERS		/* Interpreter type, for game's use */

#define INTVER 69			/* interpreter version */
								/* check VERSIONS file for complete descriptions */
void yzipvers()
{
	PTVWRD(PINTWD, INTID*256+INTVER);   /* set interpreter ID and VERsion */
}

