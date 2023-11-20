#define INTID 6		/* Interpreter type, for game's use */
#define INTVER 'I'	/* interpreter version */

/* F:  12/1/87  Use video call 0x10 to set color palette, to ensure
       winnage on EGA machines.  This is until we provide EGA support.
       12/16/87  Video call 0x10 didn't work anyway.  Full EGA support
       added (16-color 640x200 graphics for Beyond Zork), various
       bugs related to disk-scripting fixed.

   G:  12/22/87  Fix bug:  CLEAR -1 doesn't reset window size, leading
       to possible ridiculous MOREs...
   H:  1/04/88   Add support for CHRSET word (user-defined mapping
       between zbytes and ascii characters, rather than the default).
       Under GERMAN flag, add support for German special characters
       on display.  No input yet, since we don't know how.
   I:  3/17/88   Fix yet another damned buffering bug, causing lossage
       with underlined text.
       Change theory of detecting EGA, according to IBM technical reference.
       Try turning off mouse cursor whenever scrolling, to prevent
       odd occurrences on screen.
       Append to a script file if it exists, rather than overwriting it.
       Prints message if it's about to do this.
*/
