#include <stdio.h>
#include <mem.h>
#include <string.h>
#include <alloc.h>
#include <fcntl.h>

#define FBUFF_SIZE (60*1024)
#define NAME_STRING "xxxxxxxxxxxx"
#define NAME_STRINGL 12

void main( argc, argv )
	int argc;
	char *argv[];
{
	FILE *ifp, *ofp;
	int fsize;
	char *fbuff, not_found = 1, *search;

   _fmode = O_BINARY;      /* set the default open mode to NOT TEXT */

	if ( argc < 4 )
	{ /* need 3 args */
		printf("Incorrect command line:\n");
		printf("Usage: makegame <infile> <outfile> <gamename>\n");
		exit(1);
	}

	if ( (ifp = fopen( argv[1], "r" )) == NULL )
	{ /* can't open input file */
		printf("Unable to open input file: %s.\n", argv[1] );
		exit(1);
	}

	if ( (ofp = fopen( argv[2], "w+" )) == NULL )
	{ /* can't open output file */
		printf("Unable to open output file: %s.\n", argv[2] );
		exit(1);
	}

	if ( (fbuff = calloc( FBUFF_SIZE, sizeof(char) )) == NULL )
	{ /* can't get file buffer */
		printf("Unable to get file buffer!\n");
		exit(1);
	}

	do
	{ /* now read in and search */
		fsize = fread( fbuff, sizeof(char), FBUFF_SIZE, ifp );
		search = fbuff;

		while ( not_found && (search = memchr( search, 'x', fsize )) != NULL )
		{ /* see if string 'xxxxxxxx.dat' is there */
			if ( strncmp( NAME_STRING, search, NAME_STRINGL ) == 0 )
			{ /* FOUND IT */
				not_found = 0;
				strcpy( search, argv[3] );
				printf("Making data file be %s.\n", argv[3] );
			}
			search++;
		}
		fwrite( fbuff, sizeof( char ), fsize, ofp );
	} while( fsize == FBUFF_SIZE );

	fclose( ifp );
	fclose( ofp );
}




