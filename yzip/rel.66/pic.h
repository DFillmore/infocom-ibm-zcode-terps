/* length of Global file header */
#define HEADER_LEN 16

/* header flags */
#define HDF_GLOBAL_DIR 1         /* has global directory */
#define HDF_SOME_HUFFED 2        /* not need no more */
#define HDF_GLOBAL_HUFF 4        /* ditto */
#define HDF_NO_PALETTES 8        /* No palettes with pictures? */

/* local directory flags */
#define LDF_TRANSPARENT 1
#define LDF_HUFFED 2
#define LDF_ALT 4
#define LDF_TWO_COLOR 8
#define LDF_VERTICAL 16

#define MAX_LOCAL_DIR 512        /* maximum number of local files */

#define GBL_DIR_ID 0x03FF    					/* pick up ID from global directory */
#define GBL_FILE_ID(entry) ((entry)>>10)  	/* pick up file ID from global dir */

