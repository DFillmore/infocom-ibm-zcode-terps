
           Page  80,132
           Title Scroll - Verticle Scrolling Routines

;===========================================================================;
;                              Scroll source code                           ;
;                                                                           ;
;    Scroll and block clear routines for MCGA, VGA, EGA, and CGA graphics   ;
;                                                                           ;
;                              by John Fachini                              ;
;                      Inside Out Software Incorporated                     ;
;                      For exclusive use by Infocom, Inc.                   ;
;                                                                           ;
;                           Last modified:   5-12-89                        ;
;                           Version:          1.01                          ;
;                                                                           ;
;***************************************************************************;
;                                                                           ;
;                           Project specifications:                         ;
;                                                                           ;
; Graphics modes supported:         CGA   640 x 200   2 color               ;
;                                   EGA   640 x 200  16 color               ;
;                                   VGA   320 x 200 256 color               ;
;                                   MCGA  320 x 200 256 color               ;
;                                                                           ;
; Entry points:                                                             ;
;                                                                           ;
;      void cdecl scroll(x,y,width,height,lines,background)                 ;
;      unsigned int x,y;            /* upper left corner of scroll region */;
;      unsigned int width,height;   /* width, height of scroll region */    ;
;               int lines;          /* line count (negative scroll down) */ ;
;      unsigned char background;    /* fill color */                        ;
;                                                                           ;
; Global variables available at run-time:                                   ;
;                                                                           ;
;      unsigned char Display;       /* Display mode (1, 2, or 4) */         ;
;                                                                           ;
; The mission:                                                              ;
;                                                                           ;
;      Scroll region contents up or down, (based on the sign of 'lines').   ;
; If the line count to scroll is greater than or equal to the height of the ;
; graphics port, the region is cleared to the background color.  Otherwise  ;
; the scroll is completed and all blanked areas are cleared to the back-    ;
; ground color.                                                             ;
;                                                                           ;
; This source file should be assembled and it's OBJ file linked in with the ;
; TRIVIA.OBJ file.  Also note that version 1.04 or greater of Trivia is     ;
; required.                                                                 ;
;                                                                           ;
;***************************************************************************;
;                                                                           ;
;           Assembled by the Microsoft Macro Assembler version 5.1          :
;                           Options: /V /W2 /Z /Mx                          ;
;                                                                           ;
;***************************************************************************;
;                                                                           ;
;                              Revision History:                            ;
;                                                                           ;
; Version 1.00   -   Initial working code.                                  ;
; Version 1.01   -   Fixed CGA, EGA border wrap bugs                        ;
;                                                                           ;
;***************************************************************************;
;                                                                           ;
;                 First released version to Infocom:    1.00                ;
;                                                                           ;
;===========================================================================;

           .MODEL  Small,C          ;small memory model, C language

;--------------------------------------------------------------------------
;  C function interface macro
;--------------------------------------------------------------------------
@FUNC      Macro FuncName,FuncParameters
	Public FuncName
FuncName   Proc FuncParameters                    ;declare public func
           IFNB  <FuncParameters>                    ;for any and all parms
           Irp   @FuncArg,<FuncParameters>           ;state parms to masm
           EndM
           EndIf
           EndM

;--------------------------------------------------------------------------
;  Stack interface macros
;--------------------------------------------------------------------------
@@CheckRegister Macro RegisterToCheck
           IfIdnI  <RegisterToCheck>,<SI>
@SI_Saved  =     1
           EndIf
           IfIdnI  <RegisterToCheck>,<DI>
@DI_Saved  =     1
           EndIf
           IfIdnI  <RegisterToCheck>,<DS>
@DS_Saved  =     1
           EndIf
           EndM


@LINK      Macro Reg1,Reg2,Reg3
@SI_Saved  =     0                                   ;clear each pushed
@DI_Saved  =     0                                   ;register variable
@DS_Saved  =     0                                   ;this invokation
           IFNB  <Reg1>                              ;check 1 of 3
           @@CheckRegister Reg1
           EndIf
           IFNB  <Reg2>                              ;check 2 of 3
           @@CheckRegister Reg2
           EndIf
           IFNB  <Reg3>                              ;check 3 of 3
           @@CheckRegister Reg3
           EndIf
           if    @SI_Saved                           ;if var says so,
           Push  SI                                  ;push the register
           EndIf
           If    @DI_Saved
           Push  DI
           EndIf
           If    @DS_Saved
           Push  DS
           EndIf
           EndM

@UNLINK    Macro
           If    @DS_Saved                           ;in reverse order as above
           Pop   DS
           EndIf
           If    @DI_Saved
           Pop   DI
           EndIf
           If    @SI_Saved
           Pop   SI
           EndIf
           Ret                                       ;C pops parms, not me
           EndM

;--------------------------------------------------------------------------
;  Graphics card and system variable equates  
;--------------------------------------------------------------------------
USE_CGA    equ   1                  ;'Display' value for CGA 640 x 200
USE_EGA    equ   2                  ;'Display' value for EGA 640 x 200
USE_VGA    equ   4                  ;'Display' value for VGA/MCGA 320 x 200

SEG_CGA    equ   0b800h             ;Segment of CGA video memory
SEG_EGA    equ   0a000h             ;Segment of EGA video memory
SEG_VGA    equ   0a000h             ;Segment of VGA video memory

;--------------------------------------------------------------------------
;  EGA Hardware equates and macros
;--------------------------------------------------------------------------
Plane_Select equ 3c4h               ;Plane select port
Mask_Select  equ 3ceh               ;Mask/Mode select port

;----------------------------------------------------------
; Used by most Out port routines (from the IBM tech-ref)
;----------------------------------------------------------
@OutDX     Macro
           Out   DX,AL
           Inc   DX
           Mov   AL,AH
           Out   DX,AL
           EndM

;--------------------------------------------------------------------------
; Select AH planes as write-able.  Bits 0 - 3 correspond to planes 0 - 3
;--------------------------------------------------------------------------
@WritePlane Macro AHVal
           IfNB  <AHVal>
           Mov   AH,AHVal
           EndIf
           Mov   AL,2
           Mov   DX,Plane_Select
           @OutDX
           EndM

;-----------------------------------------------------------
; Enable a plane for reading: AH contains plane # (0 - 3)
;-----------------------------------------------------------
@ReadPlane Macro AHVal
           IfNB  <AHVal>
           Mov   AH,AHVal
           EndIf
           Mov   AL,4
           Mov   DX,Mask_Select
           @OutDX
           EndM

;-------------------------------------------------------------------------
; Enable writes to bits according to mask in AH (set bits are enabled)
;-------------------------------------------------------------------------
@WriteMask Macro AHVal
           IfNB  <AHVal>
           Mov   AH,AHVal
           EndIf
           Mov   AL,8
           Mov   DX,Mask_Select
           @OutDX
           EndM

;----------------------------------------------------------
; Set a write mode given mode # in AH
;----------------------------------------------------------
@WriteMode Macro AHVal
           IfNB  <AHVal>
           Mov   AH,AHVal
           EndIf
           Mov   AL,3
           Mov   DX,Mask_Select
           @OutDX
           EndM

;-------------------------------------------------------------
; Set EGA graphics to normal write operations
;-------------------------------------------------------------
@NormalMode Macro
           @WriteMode 0
           EndM

;----------------------------------------------------------------------
; Reset the state of the EGA card: all planes, all bits enabled.
;----------------------------------------------------------------------
@ResetEGA  Macro
           @NormalMode
           @WritePlane 0fh
           @WriteMask 0ffh
           EndM

;************************ The real stuff starts here *************************

           .Data
;--------------------------------------------------------------------------
;  External variable declarations
;--------------------------------------------------------------------------
           Extrn Display          : Byte           ;display type field
           Extrn GrafSeg          : Word           ;from Trivia
           Extrn YTable           : Word           ;  "     "
           Extrn EGAScratch       : Byte           ;  "     "

CoordX0    dw    ?
CoordY0    dw    ?
CoordX1    dw    ?
CoordY1    dw    ?
PicWidth   dw    ?
PicHeight  dw    ?
PicLines   dw    ?
CurrentY   dw    ?
LineCount  dw    ?
TrueWidth  dw    ?
ByteOffset dw    ?
TransColor db    ?

;--------------------------------------------------------------------------
; Mask tables
;--------------------------------------------------------------------------
EMaskLeft  Label Byte               ;EGA mask (left)
CMaskLeft  Label Byte               ;CGA mask (left)
           db    00000000b
           db    01111111b
           db    00111111b
           db    00011111b
           db    00001111b
           db    00000111b
           db    00000011b
           db    00000001b
EMaskRight Label Byte               ;EGA mask (right)
CMaskRight Label Byte               ;CGA mask (right)
           db    10000000b
           db    11000000b
           db    11100000b
           db    11110000b
           db    11111000b
           db    11111100b
           db    11111110b
           db    00000000b
;--------------------------------------------------------------------------
; Scroll specific local storage
;--------------------------------------------------------------------------
LeftMask   db    0                  ;left mask applied to hline
RightMask  db    0                  ;right mask applied to hline
DeltaY     dw    0                  ;change in Y (1 or -1)
StartY     dw    0                  ;starting Y in scroll
DestY      dw    0                  ;dest Y in scroll
BoxY0      dw    0                  ;top line of region to fill after scroll
BoxY1      dw    0                  ;bottom line of region to fill

;************************** And finally... the code *************************

           .Code

           @FUNC iscroll,<X0:Word,Y0:Word,Wth:Word,Hth:Word,Lines:Word,BColor:Byte>
           @LINK SI,DI,DS
           Push  DS                 ;mov ES,DS
           Pop   ES                 ;the hard way
           Mov   AL,BColor          ;grab background color
           Mov   TransColor,AL      ;put it in the data segment
           Mov   AX,X0              ;upper left X co-ord
           Mov   BX,Y0              ;upper left Y co-ord
           Mov   CX,Wth             ;width in pixels (1 based)
           Mov   DX,Hth             ;height in lines (1 based)
           Mov   PicWidth,CX        ;remember width and
           Mov   PicHeight,DX       ;height for later
           Add   CX,AX              ;x1 = width  + x0 - 1
           Add   DX,BX              ;y1 = height + y0 - 1
           Dec   CX                 ;adjust for inclusive
           Dec   DX                 ;value calc
           Mov   CoordX0,AX         ;who needs range checking?
           Mov   CoordY0,BX         ;J.D. wouldn't pass bad data
           Mov   CoordX1,CX         ;right?
           Mov   CoordY1,DX         ;sure
           Mov   AX,Lines           ;line count (signed)
           Mov   BX,-1              ;default to 'up'
           Or    AX,AX              ;set sign bit
           Jns   Scroll0            ;scroll up it is
           Neg   AX                 ;|ax|
           Neg   BX                 ;--1
Scroll0:
           Mov   LineCount,AX       ;# of lines to move
           Mov   DeltaY,BX          ;direction value
           Mov   CX,CoordY0         ;CX = Y0
           Mov   DX,CoordY1         ;DX = Y1
	   Cld			    ; Make sure direction is correct
;--------------------------------------------------------------------------
; Given that AX is the absolute value of the 'lines' parameter, setup the
; starting conditions for variables.
; If the scroll is 'up' (i.e. DeltaY = -1):
;          StartY  =  Y0 + AX
;          DestY   =  Y0
;          BoxY0   =  Y1 - AX + 1
;          BoxY1   =  Y1
; If the scroll is 'down' (i.e. DeltaY = 1):
;          StartY  =  Y1 - AX
;          DestY   =  Y1
;          BoxY0   =  Y0
;          BoxY1   =  Y0 + AX - 1
; After the calcs are done, make LineCount = PicHeight - LineCount
; find the true number of lines to move.  If the result is <= 0, adjust
; BoxY0 = Y0 and BoxY1 = Y1 so that the region passed is cleared.
;--------------------------------------------------------------------------
           Cmp   DeltaY,-1          ;up or down?
           Je    Scroll_Up          ;up it is
Scroll_Down:
           Mov   StartY,DX
           Sub   StartY,AX          ;starty = y1 - ax
           Mov   DestY,DX           ;desty  = y1
           Mov   BoxY0,CX           ;boxy0  = y0
           Add   CX,AX
           Dec   CX
           Mov   BoxY1,CX           ;boxy1 = y0 + ax - 1
           Jmp   Short Scroll1      ;back to generalized code
Scroll_Up:
           Mov   StartY,CX
           Add   StartY,AX          ;starty = y0 + ax
           Mov   DestY,CX           ;desty  = y0
           Mov   BoxY1,DX           ;boxy1  = y1
           Sub   DX,AX
           Inc   DX
           Mov   BoxY0,DX           ;boxy0 = y1 - ax + 1
Scroll1:
           Mov   CX,PicHeight       ;height in lines
           Sub   CX,LineCount       ;take scroll count
           Mov   LineCount,CX       ;save it back
           Cmp   CX,0               ;check for sign bit or zero
           Jg    Scroll1_1          ;no problem, it's a 'normal' scroll
           Mov   AX,CoordY0         ;top Y
           Mov   BoxY0,AX           ;for large fill
           Mov   AX,CoordY1         ;bottom Y
           Mov   BoxY1,AX           ;adjusted
;--------------------------------------------------------------------------
; Check for an initialized variable set - if it's not setup, make it so
;--------------------------------------------------------------------------
Scroll1_1:
           Cmp   GrafSeg,0          ;uninit'd?
           Jne   Scroll5            ;all set - finish scroll specific setup
;--------------------------------------------------------------------------
; Setup GrafSeg with the appropriate value
;--------------------------------------------------------------------------
           Mov   AX,SEG_CGA         ;try CGA first
           Cmp   Display,USE_CGA    ;is this it?
           Je    Scroll2            ;yup
           Mov   AX,SEG_EGA         ;try EGA next (and since EGA/VGA same...)
Scroll2:
           Mov   GrafSeg,AX         ;save graphics seg
           Xor   AX,AX              ;AX = 0
           Mov   CX,200             ;Y table counter (always 200 for now)
           Mov   DI,Offset Dgroup:YTable ;ES:DI points into it
           Mov   BX,640/8           ;EGA line offset
           Cmp   Display,USE_CGA    ;is it the CGA version?
           Je    Scroll4            ;yes
           Cmp   Display,USE_EGA    ;EGA (guessed right?)
           Je    Scroll3            ;yes
           Mov   BX,320             ;byte per pixel (it's VGA)
Scroll3:
           StoSW                    ;YTable[0..199]
           Add   AX,BX              ;skip to next line
           Loop  Scroll3            ;200 times
           Jcxz  Scroll5            ;and continue with the Scroll code
Scroll4:
           Mov   DX,AX              ;current line number
           Mov   DH,DL              ;multiply by 256 (8 shifts left)
           And   DX,1feh            ;force into range (1 shifted with clr 0b)
           Shl   DX,1               ;*512
           Shl   DX,1               ;*1024
           Shl   DX,1               ;*2048
           Mov   BX,DX              ;working copy
           And   BH,7               ;gimmie lower 3 bits
           Shl   DX,1               ;still higher
           Shl   DX,1               ;last one (promise)
           Add   BX,DX              ;we've got interlace and offset now
           Xchg  AX,BX              ;setup AX as wanted value
           StoSW                    ;save the crazy result
           Xchg  AX,BX              ;back again to AX
           Inc   AX                 ;next line 0..199
           Loop  Scroll4            ;that's 200 lines
;--------------------------------------------------------------------------
; Lookup the masks and solid byte count for each line to be moved.
;--------------------------------------------------------------------------
Scroll5:
           Mov   LeftMask,0         ;reset the
           Mov   RightMask,0        ;mask values to none
           Mov   ES,GrafSeg         ;graphics segment in use
           Cmp   Display,USE_CGA    ;CGA?
           Je    Scroll_CGA         ;good guess
           Cmp   Display,USE_VGA    ;VGA?
           Je    Scroll_VGA         ;sure is
           Jmp   Scroll_EGA         ;default
;--------------------------------------------------------------------------
; Scroll_VGA: since VGA is a byte per pixel, 320 bytes per line linear
; addressing, there's no masking to set up.  The line width is PicWidth
; bytes.
;--------------------------------------------------------------------------
Scroll_VGA:
           Cmp   LineCount,0        ;scroll and fill or just fill?
           Jle   Scroll_VGA_3       ;just the box
;--------------------------------------------------------------------------
; Copy lines from StartY to DestY for LineCount lines.
;--------------------------------------------------------------------------
           Mov   BX,320             ;line offset (320 bytes per line)
           Cmp   DeltaY,-1          ;moving down through the buffer (up scr?)
           Je    Scroll_VGA_0       ;yes, use down offset
           Neg   BX                 ;-320
Scroll_VGA_0:
           Mov   SI,StartY          ;starting Y
           Mov   DI,DestY           ;where to copy it to
           Mov   DX,LineCount       ;# of lines to move
           Shl   SI,1               ;word lookup
           Shl   DI,1               ;word lookup
           Mov   SI,YTable[SI]      ;starting offset
           Mov   DI,YTable[DI]      ;starting offset
           Add   SI,CoordX0         ;adjust inward for X0
           Add   DI,CoordX0         ;same here
           Mov   AX,PicWidth        ;buffer width (byte per pixel)
           Push  ES                 ;DS and ES
           Pop   DS                 ;must both be graphics segment
Scroll_VGA_1:
           Mov   CX,AX              ;pixel count in bytes
           Push  SI                 ;save source index
           Push  DI                 ;and dest index
           Rep   MovSB              ;copy the lines
           Pop   DI                 ;restore index
           Pop   SI                 ;source too
           Add   DI,BX              ;adjust line offset
           Add   SI,BX              ;both places
           Dec   DX                 ;one less line
           Jne   Scroll_VGA_1       ;until all lines moved
           Push  SS                 ;recover C's DS
           Pop   DS                 ;by the SS method
;--------------------------------------------------------------------------
; Fill in a solid box from (BoxY0,CoordX0) (BoxY1,CoordX1) in TransColor.
;--------------------------------------------------------------------------
Scroll_VGA_3:                       ;clear box to TransColor
           Mov   DX,BoxY1           ;ending Y
           Mov   DI,BoxY0           ;'top' of fill area
           Sub   DX,DI              ;find difference (0 based) line count
           Shl   DI,1               ;word lookup
           Mov   DI,YTable[DI]      ;starting offset (ES:DI)
           Add   DI,CoordX0         ;X coord offset from left edge
           Mov   AL,TransColor      ;background color to fill with
           Mov   BX,320             ;line offset fixed at 320 bytes
Scroll_VGA_4:
           Mov   SI,DI              ;preserve starting DI for each line
           Mov   CX,PicWidth        ;byte per pixel
           Rep   StoSB              ;fill in the line
           Mov   DI,SI              ;restore starting offset
           Add   DI,BX              ;skip down to next line in the box
           Dec   DX                 ;one less line to clear
           Jns   Scroll_VGA_4       ;until all lines filled in
           Jmp   Exit               ;and return

;--------------------------------------------------------------------------
; Scroll_CGA: Find the left and right masks, the solid byte count in
; the middle of the line segment, and then do scroll/fill on those lines.
;--------------------------------------------------------------------------
Scroll_CGA:
           Mov   AX,PicWidth        ;width
           Mov   TrueWidth,AX       ;setup
           Mov   BX,CoordX0         ;starting X
           And   BX,7               ;mod 8 for table lookup
           Mov   AL,CMaskLeft[BX]   ;mask for left edge
           Mov   CX,8               ;8 - X0 mod 8
           Sub   CX,BX              ;are pixels used
           And   CX,7               ;mod 8 again
           Sub   TrueWidth,CX       ;pixel count
           Mov   LeftMask,AL        ;save it
           Mov   BX,CoordX1         ;right edge
           Mov   DX,BX              ;remember X1 for later
           And   BX,7               ;mod 8
           Mov   AL,CMaskRight[BX]  ;right edge
           Mov   RightMask,AL       ;save this one too
           Inc   BX                 ;+1
           And   BX,7               ;mod 8
           Sub   TrueWidth,BX       ;pixels used on right
           Mov   CL,3               ;2^3 = 8; 8 pixels per byte
           Mov   BX,CoordX0         ;DX = x1, BX = x0
           Shr   BX,CL              ;find which byte to start in
           Shr   DX,CL              ;and ending
           Mov   ByteOffset,BX      ;remember starting offset
           Shr   TrueWidth,1        ;truewidth /= 8
           Shr   TrueWIdth,1
           Shr   TrueWidth,1        ;to get # of solid bytes in line
           Cmp   BX,DX              ;same byte, starting & ending?
           Jne   Scroll_CGA_0       ;nope
           Mov   TrueWidth,0        ;yes
           Mov   AL,RightMask       ;intersection of masks
           Or    AL,AL              ;zero already?
           Je    Scroll_CGA_0       ;yes
           And   LeftMask,AL        ;is what's needed then
           Mov   RightMask,0        ;clear it
;--------------------------------------------------------------------------
; Check for box only, or scroll and box
;--------------------------------------------------------------------------
Scroll_CGA_0:
           Mov   BL,LeftMask        ;left side mask
           Mov   BH,RightMask       ;right side mask
           Cmp   LineCount,0        ;fill?
           Jle   Scroll_CGA_5       ;yes, forget scroll
;--------------------------------------------------------------------------
; CGA scroll
;--------------------------------------------------------------------------
Scroll_CGA_1:
           Mov   SI,StartY          ;current Y
           Mov   DI,DestY           ;ending Y
           Shl   SI,1               ;word lookup
           Shl   DI,1               ;on both
           Mov   SI,YTable[SI]      ;get true offsets from ES
           Mov   DI,YTable[DI]      ;on both
           Add   SI,ByteOffset      ;move in from left edge
           Add   DI,ByteOffset      ;bytes
           Mov   CX,TrueWidth       ;solid width
           Push  ES                 ;get DS == ES
           Pop   DS                 ;both graph seg
           Or    BL,BL              ;anything to mask on left?
           Je    Scroll_CGA_2       ;nope
           LodSB                    ;AL = source data
           And   AL,BL              ;strip unwanted bits
           Not   BL                 ;invert the mask
           And   ES:[DI],BL         ;clear dest bits
           Not   BL                 ;back to normal
           Or    ES:[DI],AL         ;write in new data
           Inc   DI                 ;and skip to next byte
Scroll_CGA_2:
           Jcxz  Scroll_CGA_3       ;no solid bytes in this line
           Rep   MovSB              ;there are... move me
Scroll_CGA_3:
           Or    BH,BH              ;right mask?
           Je    Scroll_CGA_4       ;nope
           LodSB                    ;source byte again
           And   AL,BH              ;right side this time
           Not   BH                 ;invert
           And   ES:[DI],BH         ;clear dest bits
           Not   BH                 ;restored
           Or    ES:[DI],AL         ;write in new bits
Scroll_CGA_4:
           Push  SS                 ;get C's
           Pop   DS                 ;DS back
           Mov   AX,DeltaY          ;offset value (1 or -1)
           Sub   StartY,AX          ;start and
           Sub   DestY,AX           ;end
           Dec   LineCount          ;one less to do
           Jne   Scroll_CGA_1       ;back to top
;--------------------------------------------------------------------------
; Now do the box fill
;--------------------------------------------------------------------------
Scroll_CGA_5:
           Mov   CX,TrueWidth       ;solid width in bytes
           Mov   DI,BoxY0           ;working towards Y1
           Shl   DI,1               ;word lookup
           Mov   DI,YTable[DI]      ;found
           Add   DI,ByteOffset      ;move in
           Mov   AH,TransColor      ;pixel value
           Or    AH,AH              ;zero or non-zero?
           Je    Scroll_CGA_6       ;zero
           Mov   AH,0ffh            ;set all bits
Scroll_CGA_6:
           Or    BL,BL              ;left mask?
           Je    Scroll_CGA_7       ;yes, forget it
           Mov   AL,AH              ;copy value
           And   AL,BL              ;strip bits
           Not   BL                 ;invert
           And   ES:[DI],BL         ;clear
           Or    ES:[DI],AL         ;set
           Inc   DI                 ;next byte
           Not   BL                 ;reset
Scroll_CGA_7:
           Jcxz  Scroll_CGA_8       ;no solid bytes
           Mov   AL,AH              ;fill value
           Rep   StoSB              ;write 'em in
Scroll_CGA_8:
           Or    BH,BH              ;right mask
           Je    Scroll_CGA_9       ;not there
           Mov   AL,AH              ;copy value
           And   AL,BH              ;mask
           Not   BH                 ;invert
           And   ES:[DI],BH         ;strip
           Or    ES:[DI],AL         ;set
           Not   BH                 ;reset the mask
Scroll_CGA_9:
           Inc   BoxY0              ;next Y down
           Mov   AX,BoxY1           ;copy
           Cmp   BoxY0,AX           ;done?
           Jle   Scroll_CGA_5       ;not yet
           Jmp   Exit               ;see ya

;--------------------------------------------------------------------------
; Scroll_EGA: Find the left and right masks, the solid byte count in
; the middle of the line segment, and then do scroll/fill on those lines.
;--------------------------------------------------------------------------
Scroll_EGA:
           Mov   AX,PicWidth        ;width
           Mov   TrueWidth,AX       ;setup
           Mov   BX,CoordX0         ;starting X
           And   BX,7               ;mod 8 for table lookup
           Mov   AL,EMaskLeft[BX]   ;mask for left edge
           Mov   CX,8               ;8 - X0 mod 8
           Sub   CX,BX              ;are pixels used
           And   CX,7               ;mod 8 again
           Sub   TrueWidth,CX       ;pixel count
           Mov   LeftMask,AL        ;save it
           Mov   BX,CoordX1         ;right edge
           Mov   DX,BX              ;remember X0 for later
           And   BX,7               ;mod 8
           Mov   AL,EMaskRight[BX]  ;right edge
           Mov   RightMask,AL       ;save this one too
           Inc   BX                 ;+1
           And   BX,7               ;mod 8
           Sub   TrueWidth,BX       ;pixels used on right
           Mov   CL,3               ;2^3 = 8; 8 pixels per byte
           Mov   BX,CoordX0         ;DX = x1, BX = x0
           Shr   BX,CL              ;find which byte to start in
           Shr   DX,CL              ;and ending
           Mov   ByteOffset,BX      ;remember starting offset
           Shr   TrueWidth,1        ;truewidth /= 8
           Shr   TrueWidth,1
           Shr   TrueWidth,1        ;to get # of solid bytes in line
           Cmp   BX,DX              ;same byte, starting & ending?
           Jne   Scroll_EGA_0       ;nope
           Mov   TrueWidth,0        ;yes
           Mov   AL,RightMask       ;intersection of masks
           Or    AL,AL              ;zero already?
           Je    Scroll_EGA_0       ;yes
           And   LeftMask,AL        ;is what's needed then
           Mov   RightMask,0        ;clear it
;--------------------------------------------------------------------------
; Check for box only, or scroll and box
;--------------------------------------------------------------------------
Scroll_EGA_0:
           Mov   BL,LeftMask        ;left side mask
           Mov   BH,RightMask       ;right side mask
           Cmp   LineCount,0        ;fill?
           Jg    Scroll_EGA_1       ;scroll and fill
           Jmp   Scroll_EGA_6       ;fill only
;--------------------------------------------------------------------------
; EGA scroll: start by copying the source line into EGAScratch
;--------------------------------------------------------------------------
Scroll_EGA_1:
           Mov   SI,StartY          ;current Y
           Shl   SI,1               ;word lookup
           Mov   SI,YTable[SI]      ;offset
           Mov   DI,Offset Dgroup:EGAScratch ;base internal line buffer
           Xor   AH,AH              ;starting plane = 0
           Push  ES                 ;set DS = video buffer
           Pop   DS                 ;done
           Push  SS                 ;set ES
           Pop   ES                 ;to C's DS for ES:DI EGAScratch
Scroll_EGA_1_0:
           @ReadPlane               ;read plane AH (0..3)
           Mov   AL,[SI]            ;latch
           Mov   CX,640/8           ;bytes per line
           Rep   MovSB              ;grab pels
           Sub   SI,640/8           ;adjust backwards
           Inc   AH                 ;next plane
           Cmp   AH,4               ;done yet?
           Jne   Scroll_EGA_1_0     ;nope
           @ResetEGA                ;yes, clean up card for now
           Push  ES                 ;push C's DS on stack
           Push  DS                 ;graf seg
           Pop   ES                 ;ES = graf seg
           Pop   DS                 ;DS = C's DS
           Mov   DI,DestY           ;ending Y
           Shl   DI,1               ;word lookup
           Mov   DI,YTable[DI]      ;offset from ES
           Mov   SI,Offset Dgroup:EGAScratch ;source data by plane
           Add   SI,ByteOffset      ;skip in this time
           Add   DI,ByteOffset      ;bytes too
           Mov   AH,1               ;plane counter
Scroll_EGA_2:
           Mov   CX,TrueWidth       ;solid width
           Push  AX                 ;save plane counter
           Push  SI                 ;save source index
           Push  DI                 ;and dest
           @WritePlane              ;enable plane AH
           Or    BL,BL              ;left mask?
           Je    Scroll_EGA_3       ;nope
           @WriteMask BL            ;bit mask
           Mov   AL,ES:[DI]         ;latch
           MovSB                    ;copy this plane's data with protect
Scroll_EGA_3:
           Jcxz  Scroll_EGA_4       ;no solid bytes in middle
           @WriteMask 0ffh          ;all bits
           Mov   AL,ES:[DI]         ;latch
           Rep   MovSB              ;copy
Scroll_EGA_4:
           Or    BH,BH              ;right side mask?
           Je    Scroll_EGA_5       ;nothing
           @WriteMask BH            ;set mask with protect
           Mov   AL,ES:[DI]         ;latch
           MovSB                    ;this plane
Scroll_EGA_5:
           Pop   DI                 ;restore source (since plane * 4)
           Pop   SI                 ;source pointer into EGAScratch
           Pop   AX                 ;recover plane counter from TOS
           Add   SI,640/8           ;bytes per line (don't touch DI)
           Shl   AH,1               ;next plane
           Cmp   AH,10h             ;done with planes?
           Jne   Scroll_EGA_2       ;not yet, do all 4 planes
           Mov   AX,DeltaY          ;offset value (1 or -1)
           Sub   StartY,AX          ;start and
           Sub   DestY,AX           ;end
           Dec   LineCount          ;one less to do
           Je    Scroll_EGA_6       ;done
           Jmp   Scroll_EGA_1       ;back for more
;--------------------------------------------------------------------------
; EGA box fill (CoordX0,BoxY0) (CoordX1,BoxY1) in TransColor
;--------------------------------------------------------------------------
Scroll_EGA_6:
           Mov   DI,BoxY0           ;starting Y in the box
           Shl   DI,1               ;*2
           Mov   DI,YTable[DI]      ;lookup
           Add   DI,ByteOffset      ;skip in
           Mov   CX,TrueWidth       ;solid byte count
           Or    BL,BL              ;left mask
           Je    Scroll_EGA_8       ;nothing to mask
           @WriteMask BL            ;enable bits in mask
           @WritePlane 0fh          ;all planes
           Mov   AL,ES:[DI]         ;latch plane
           Xor   AL,AL              ;black out
           Mov   ES:[DI],AL         ;left side
           Mov   AH,TransColor      ;color to fill with
           Or    AH,AH              ;black already?
           Je    Scroll_EGA_7       ;yes
           @WritePlane              ;setup the colors
           Mov   AL,0ffh            ;all bits
           Mov   ES:[DI],AL         ;write thru
Scroll_EGA_7:
           Inc   DI                 ;next screen byte
Scroll_EGA_8:
           Jcxz  Scroll_EGA_9       ;no solid bytes in midline
           @WriteMask 0ffh          ;all bits
           @WritePlane 0fh          ;all planes
           Push  DI                 ;preserve
           Push  CX                 ;these for pass 2
           Xor   AL,AL              ;black
           Rep   StoSB              ;nuke em
           Pop   CX                 ;byte counter back
           Pop   DI                 ;and offset
           Mov   AH,TransColor      ;color to fill with
           Or    AH,AH              ;black?
           Je    Scroll_EGA_9       ;no problem
           @WritePlane              ;pick planes in the color
           Mov   AL,0ffh            ;all bits
           Rep   StoSB              ;write the color into memory
           Jcxz  Scroll_EGA_10      ;skip adjust for no second stosb
Scroll_EGA_9:
           Add   DI,CX              ;adjust as if second stosb was done
Scroll_EGA_10:
           Or    BH,BH              ;right side mask this time
           Je    Scroll_EGA_11      ;nothing to do
           @WriteMask  BH           ;yet again
           @WritePlane 0ffh         ;all planes with protected bits
           Mov   AL,ES:[DI]         ;latch
           Xor   AL,AL              ;black
           Mov   ES:[DI],AL         ;write it in
           Mov   AH,TransColor      ;fill color
           Or    AH,AH              ;black?
           Je    Scroll_EGA_11      ;yes, get out
           @WritePlane              ;color planes selected
           Mov   AL,0ffh            ;all bits
           Mov   ES:[DI],AL         ;nuke non-protected bits
Scroll_EGA_11:
           @ResetEGA                ;cleanup temp
           Inc   BoxY0              ;next line down
           Mov   AX,BoxY0           ;get it locally
           Cmp   AX,BoxY1           ;done yet?
           Jg    Exit               ;yup
           Jmp   Scroll_EGA_6       ;nope, back for more
Exit:
           @UNLINK
iscroll     EndP

           End

;----------------------------------------------------------------------------

