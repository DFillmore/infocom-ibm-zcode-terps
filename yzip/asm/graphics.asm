           Page  80,132
           Title Trivia - A Graphics Blit/Compression Software Package

;===========================================================================;
;                       'Trivia' project source code                        ;
;                                                                           ;
;  Compression and graphics routines for MCGA, VGA, EGA, and CGA graphics   ;
;                                                                           ;
;                              by John Fachini                              ;
;                      Inside Out Software Incorporated                     ;
;                      For exclusive use by Infocom, Inc.                   ;
;                                                                           ;
;                           Last modified:   May 12, 1989                   ;
;                           Version:         1.11                           ;
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
;      void cdecl disp_pic_f(x,y,width,height,rx,by,handle,transparent)     ;
;      unsigned int x,y;            /* upper left corner of blit region */  ;
;      unsigned int width,height;   /* width, height of graphics image */   ;
;      unsigned int rx,by;          /* lower right point of blit region */  ;
;      unsigned int handle;         /* file handle of open data file */     ;
;      unsigned char transparent;   /* Transparent color (0xff if none)     ;
;                                                                           ;
;      void cdecl disp_pic_b(x,y,width,height,rx,by,bptr,transparent)       ;
;      unsigned int x,y;            /* upper left corner of blit region */  ;
;      unsigned int width,height;   /* width, height of graphics image */   ;
;      unsigned int rx,by;          /* lower right point of blit region */  ;
;      unsigned char far *bptr;     /* 32 bit address of pre-loaded image */;
;      unsigned char transparent;   /* Transparent color (0xff if none)     ;
;                                                                           ;
; Global variables available at run-time:                                   ;
;                                                                           ;
;      void far *Hash_buff;         /* 12K LZ hash landing area */          :
;      void far *Pic_buff;          /* available buffer pre-allocated */    ;
;      unsigned int Pic_buff_size;  /* size in paragraphs of Pic_buff */    ;
;      unsigned char Display;       /* Display mode (1, 2, or 4) */         ;
;                                                                           ;
; The mission:                                                              ;
;                                                                           ;
;      Blit a compressed image from buffer or file into the indicated       ;
;      screen region, applying transparent color and clipping to the lower  ;
;      right bounds passed in.                                              ;
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
; Version 1.01   -   Modified to perform blits by line rather than by plane ;
;                    when in EGA 640x200 mode.                              ;
; Version 1.02   -   Uses half of Pic_Buff for EGA buffering instead of     ;
;                    segment B000 to ensure compatibility with clone EGA    ;
;                    cards.                                                 ;
; Version 1.03   -   Blits one line at a time and leaves Pic_buff to full   ;
;                    size.                                                  ;
;                                                                           ;
; Version 1.10   -   Fixed for assembly under Turbo ASM:                    ;
;                       Changed "Extrn C" to just "Extrn"                   ;                        
;                       Changed "Proc C" to just "Proc" in FUNC macro       ;
;                       Add "Publc FuncName" to FUNC macro                  ;
;                       Add "Proc Near" to all functions                    ;
;                        and all matching EndP's                            ;
;                       Make GrafInit be one Proc                           ;
;									    ;
; Version 1.11  -   Made a YTable, GrafSeg, and EGAScratch Public for the   ;
;			scrolling routines				    ;
;									    ;
;***************************************************************************;
;                                                                           ;
;                 First released version to Infocom:    1.03                ;
;                                                                           ;
;***************************************************************************;
;                                                                           ;
;            "Trivia" - an album by Utopia featuring Todd Rundgren          ;
;              Copyright (C) 1986 Utopia and Passport Records Inc.          ;
;                                                                           ;
;===========================================================================;

           .MODEL  Small,C          ;small memory model, C language

;--------------------------------------------------------------------------
;  C function interface macro
;--------------------------------------------------------------------------
@FUNC      Macro FuncName,FuncParameters
        Public FuncName
FuncName   Proc  FuncParameters                    ;declare public func
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

EGA_SEGOFS equ   1000h              ;paragraphs to skip to second video page

BufUnpack  equ   0                  ;unpack from buffer mode
FileUnpack equ   1                  ;unpack from input file

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

;--------------------------------------------------------------------------
; SetALPixel - set plane layout at ES:DI as bits in lo nibble of AL
;              All registers preserved.
;--------------------------------------------------------------------------
@SetALPixel Macro
           Local P1Check,P2Check,P3Check,SetExit
           Not   DL                 ;convert to one clear bit
           And   ES:[DI    ],DL     ;clear all 4 bit planes by default
           And   ES:[DI+ 80],DL     ;2 of 4
           And   ES:[DI+160],DL     ;3 of 4
           And   ES:[DI+240],DL     ;4 of 4
           Not   DL                 ;reset the bit mask
           Test  AL,1               ;plane 0 need setting?
           Je    P1Check            ;no, check plane 1
           Or    ES:[DI],DL         ;set it
P1Check:
           Test  AL,2               ;plane 1 need setting?
           Je    P2Check            ;no, check plane 2
           Or    ES:[DI+80],DL      ;set it
P2Check:
           Test  AL,4               ;plane 2 need setting?
           Je    P3Check            ;no, check plane 3
           Or    ES:[DI+160],DL     ;set it
P3Check:
           Test  AL,8               ;plane 3 need setting?
           Je    SetExit            ;done
           Or    ES:[DI+240],DL     ;set it
SetExit:
           EndM
           
;--------------------------------------------------------------------------
;  MS-DOS macros and equates for use with the LZ decompression
;--------------------------------------------------------------------------
@Mov32     Macro Dest32,Src32
           Mov   AX,Word Ptr Src32
           Mov   Word Ptr Dest32,AX
           Mov   AX,Word Ptr Src32+2
           Mov   Word Ptr Dest32+2,AX
           EndM

@Normalize Macro Ptr32
           Mov   AX,Word Ptr Ptr32  ;offset in 32 bit field
           Mov   CL,4               ;shift factor
           Shr   AX,CL              ;get rid of lo nibble, shift other 12 down
           And   AX,0fffh           ;for neatness sake
           Add   Word Ptr Ptr32+2,AX ;add in the remnants of the oversized ofs
           And   Word Ptr Ptr32,0fh ;and force the offset in range 00..0f
           EndM

@Dos_Int   Macro CmdCode
           Mov   AH,CmdCode
           Int   21h
           EndM

@FRead     Macro Handle,ReadLength
           IfNB  <Handle>
                 IfDifI <BX>,<Handle>
                 Mov   BX,Handle
                 EndIf
           EndIf
           IfNB <ReadLength>
                 IfDifI <CX>,<ReadLength>
                 Mov   CX,ReadLength
                 EndIf
           EndIf
           @Dos_Int 3fh
           EndM

;--------------------------------------------------------------------------
; LZ decompression constants
;--------------------------------------------------------------------------
Clr_Token  equ   256                ;clear hash table token
Eof_Token  equ   257                ;end of compressed stream token
FF_Token   equ   258                ;first free hash entry token

;--------------------------------------------------------------------------
;  LZ hash table structure 
;--------------------------------------------------------------------------
HashStruct Struc
Next       dw    ?
Char       db    ?
HashStruct EndS

;--------------------------------------------------------------------------
; LZ macros for much improved decompression performance.  These were
; subroutines once but the routine proved too slow on 4.77 MHz machines.
; Also note: the register assumptions during the execution of the LZ
; decompression loop are minimal:
;          DS : C's data segment
; That's it.
; Wake up J.D.!
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
; @Index: Return BX * 3 (3 = sizeof(HashStruc))
;--------------------------------------------------------------------------
@Index     Macro
           Push  AX                 ;preserve AX
           Mov   AX,BX              ;temp storage
           Shl   BX,1               ;BX * 2
           Add   BX,AX              ;add makes it *3
           Pop   AX                 ;restore AX
           EndM

;--------------------------------------------------------------------------
; @Add_Code: Add a code to the hash table.
;            On exit, AX, BX, ES, and DI are trashed
;--------------------------------------------------------------------------
@Add_Code  Macro
           Mov   BX,FreeCode        ;BX = next free code from local storage
           @Index                   ;build index into hash table
           Les   DI,Hashbuff        ;12K hash area address
           Add   DI,BX              ;add in the hash index
           Mov   AL,K               ;setup for the move
           Mov   ES:[DI].Char,AL    ;save the char in the hash rec
           Mov   AX,OldCode         ;linkage
           Mov   ES:[DI].Next,AX    ;as next ptr
           Inc   FreeCode           ;next one is free
           EndM

;--------------------------------------------------------------------------
; @Init_Table: setup local vars for table initialization
;--------------------------------------------------------------------------
@Init_Table Macro
           Mov   NBits,9            ;Starting with 9 bit codes
           Mov   MaxCode,512        ;10 0000 0000 b
           Mov   FreeCode,FF_Token  ;Reset next code free entry
           EndM

;--------------------------------------------------------------------------
; @CGA_Write: Store a block of 8 pixels into display memory.  Trashes DI,CX
;	      Counts bytes written, watches for picture width to be hit,
;	      and advances to the next line.
;-------------------------------------------------------------------------
@CGA_Write Macro
	Local	CGAW_2,CGAW_1,CGAW_0,CGAW_3,CGAW_4,CGAW_5,CGAW_6
	Cmp	CGA_Shift,0		;Faster if we're byte-aligned
	 Jne	CGAW_0
	Les	DI,Tempbuff
	Dec	CGA_Left
	 Jz	CGAW_4			;Jump if last byte on line
	StoSB
	Inc	Word Ptr Tempbuff
	Jmp	CGAW_2
CGAW_4:	Mov	AH,ES:[DI]  		;Pick up screen byte
	Mov	DI,CGA_Trail		; And mask off the unused part
	And	AH,CGA_Masks[DI]
	Or	AL,AH
	Mov	DI,Tempbuff
	StoSB				;Write out the last one
	Jmp	Short CGAW_3		;And move to the next line

CGAW_0:	Mov	CL,CGA_Shift		;Current stuff ends up low in AL;
	Ror	AX,CL			;Stuff for next byte high in AH
	Mov	DI,CGA_Shift
	And	AL,CGA_Masks[DI]	;Clear the high part in AL
	Or	AL,CGA_Temp		;Combine with what we had saved
	And	AH,CGA_Omasks[DI]	;Clear the low part in AH
	Mov	CGA_Temp,AH		;Save for next time
	Les	DI,Tempbuff
	Dec	CGA_Left		;Check for end of line
	 Jz	CGAW_5			;Yes, extra hair
	StoSB	      			; Store the byte (normal case)
	Inc	Word Ptr Tempbuff
	Jmp	Short CGAW_2
CGAW_5:	Cmp	CGA_Trail,0
	 Jge	CGAW_6
	StoSB				; Store a byte
	Inc	Word Ptr Tempbuff
	Mov	AL,ES:[DI]		; Pick up next screen byte
	Mov	DI,CGA_Trail
	Neg	DI
	And	AL,CGA_Masks[DI]
	Or	AL,CGA_Temp
	Mov	DI,Tempbuff
	StoSB
	Jmp	Short CGAW_3
CGAW_6:	Mov	AH,ES:[DI]		;Yes, pick up the next byte from the screen
	Mov	DI,CGA_Trail		;So we can dump our partial byte
	And	AH,CGA_Masks[DI]
	Or	AH,CGA_Temp
	Or	AL,AH
	Mov	DI,Tempbuff
	StoSB
CGAW_3:	Inc	CGA_Curline		;Current line
	Mov	DI,CGA_Curline
	Shl	DI,1
	Mov	CX,Ytable[DI] 		;Beginning of display memory
	Add	CX,CGALStart		;Beginning of this picture
	Mov	Tempbuff,CX
	Mov	CX,CGA_Len
	Mov	CGA_Left,CX
	Cmp	CGA_Shift,0
	 Je	CGAW_2
	Mov	DI,Tempbuff
	Mov	AL,ES:[DI]		;Pick up the first byte
	Mov	DI,CGA_Shift		;And save the high part
;	Ror	AX,CL
;	Mov	DI,CGA_Shift
	And	AL,CGA_Omasks[DI]
	Mov	CGA_Temp,AL
CGAW_2:
	EndM	

;--------------------------------------------------------------------------
; @Write_Char: Store the decompressed byte in AL into the dest buffer.
;              On exit, DI is trashed.  The modification made to this
;              routine counts the bytes written, watches for the
;              calculated byte limit to be hit and the graphics blit
;              is forced.  Normally the graphics code (noted below)
;              would not appear in the macro.
;--------------------------------------------------------------------------
@Write_Char Macro
           Local Write_Char_1,Write_Char_2
           Les   DI,Tempbuff        ;32 bit dest for the byte
           StoSB                    ;write the new byte into the buffer
           Inc   Word Ptr Tempbuff  ;next offset
; Note--we never need to normalize Tempbuff, because it can never overflow
; 64K, and we don't compare different copies of it
;           Cmp   DI,16              ;time to normalize the 32 bit pointer?
;           Jne   Write_Char_1       ;no
;           Inc   Word Ptr Tempbuff+2  ;increment the segment pointer
;           Mov   Word Ptr Tempbuff,0  ;with offset 0
Write_Char_1:
           Inc   ByteCount          ;another byte's been written
           Mov   AX,ByteCount
           Cmp   AX,PicLimit        ;time to do the blit?
           Jne   Write_Char_2       ;nope
           Call  Blit               ;go for it
Write_Char_2:
           EndM

;--------------------------------------------------------------------------
; @Read_Code: process a code from the input stream
;--------------------------------------------------------------------------
@Read_Code Macro
           Local Read_Code_0,Read_Code_1,Read_Code_2,Read_Code_3,Read_Code_4,read_code_98,read_code_99
           Mov   AX,BitOffset       ;current bit offset
           Add   AX,NBits           ;add in current width
           Xchg  BitOffset,AX       ;swap em
           Mov   CX,8               ;Find byte/bit offset
           Xor   DX,DX              ;clear DX (MSW for div)
           Div   CX                 ;AX = result, DX remainder
           Cmp   AX,BUFSIZE-3       ;Nearing buffer full?
           Jl    Read_Code_0        ;no
           Push  DX                 ;save bits remaining calc
           Add   DX,NBits           ;Add bits to bits
           Mov   BitOffset,DX       ;new bit offset
           Mov   CX,BUFSIZE         ;buffer size
           Push  AX                 ;Save byte offset
           Sub   CX,AX              ;bytes left
           Push  DS
           Pop   ES                 ;ES,DS both C's DS
           Mov   SI,Offset Dgroup:Buffer
           Mov   DI,SI              ;base pointers for both
           Add   SI,AX              ;source skips ahead
           Rep   MovSB              ;'slide' the buffer
           Pop   CX                 ;pop byte offset (# of bytes to refill)
           Cmp   PickUnpack,FileUnpack ;use file?
           Je    Read_Code_3        ;yup
           Push  DS                 ;save C's DS
           Lds   SI,InputBuff       ;input buffer points to mem source
           Rep   MovSB              ;do 'read'
           Pop   DS                 ;restore me
           Mov   Word Ptr InputBuff,SI ;update to the new source offset
           @Normalize InputBuff     ;and normalize it
           Jmp   Short Read_Code_4  ;continue
Read_Code_3:
           Mov   DX,DI              ;pointer to read's dest
           @Fread FileHandle        ;gimmie more
Read_Code_4:
           Pop   DX                 ;get bit offset back from TOS
           Xor   AX,AX              ;read at start of buffer again (offset 0)
Read_Code_0:
           Mov   SI,Offset Dgroup:Buffer
           Add   SI,AX              ;offset into read buf
           LodSW                    ;get 'next' from record
           Mov   BX,AX              ;save it here
           LodSB                    ;get 'char'
           Mov   CX,DX              ;setup loop usage
           Jcxz  Read_Code_2        ;no need to do shifting
Read_Code_1:
           Shr   AL,1               ;drop lo bit into carry
           Rcr   BX,1               ;catch it here
           Loop  Read_Code_1        ;let CX say when to stop
Read_Code_2:
           Mov   AX,BX              ;what's left here we need too
           Mov   BX,NBits           ;bit count
           Sub   BX,9               ;generate index 0..3 inclusive
           Shl   BX,1               ;convert to word index
           And   AX,Masks[BX]       ;leave 9,10,11, or 12 bits
           EndM


;--------------------------------------------------------------------------
; Standardize entry macro for the "Trivia" routine
;--------------------------------------------------------------------------
@StandardEntry Macro
           Mov   AX,X0              ;upper left X in AX
           Mov   BX,Y0              ;upper left Y in BX
           Mov   CX,X1              ;lower right X in CX
           Mov   DX,Y1              ;lower right Y in CX
           Mov   SI,W               ;picture width in SI
           Mov   DI,H               ;picture height in DI
           EndM


;************************ The real stuff starts here *************************

           .Data
;--------------------------------------------------------------------------
;  External variable declarations
;--------------------------------------------------------------------------
           Extrn Hash_buff        : DWord          ;pointer to 12K hash area
           Extrn Pic_buff         : DWord          ;pointer to scratch area
           Extrn Pic_buff_size    : Word           ;size in para of Pic_buff
           Extrn Display          : Byte           ;display type field

	   Public YTable
	   Public GrafSeg
	   Public EGAScratch

BUFSIZE    equ   1024               ;Must be at least 1K and an even number
;--------------------------------------------------------------------------
;  "Trivia"'s kept variable space (sorry so much...)
;--------------------------------------------------------------------------
Buffer     db    BUFSIZE dup (?)    ;landing area for buffer input
EGAScratch db    80 * 4 dup (?)     ;bytes per line times 4 bit planes
Masks      dw    0000000111111111b  ;LZ bit masks (9 bits)
           dw    0000001111111111b  ;10 bits
           dw    0000011111111111b  ;11 bits
           dw    0000111111111111b  ;12 bits
BitOffset  dw    ?                  ;current bit offset
CurCode    dw    ?                  ;current code
OldCode    dw    ?                  ;last code processed
InCode     dw    ?                  ;code in use
FreeCode   dw    ?                  ;next free code for hash table entry
StackCount dw    ?                  ;count of pushed words
MaxCode    dw    ?                  ;max usable code this pass
NBits      dw    ?                  ;number of bits in current code
ByteCount  dw    ?                  ;number of bytes written since last blit
FinalChar  db    ?                  ;last unpacked
K          db    ?                  ;current unpacked

           Even
;--------------------------------------------------------------------------
; Graphics driver variable space
;--------------------------------------------------------------------------
GrafSeg    dw    0                  ;active graphics segment
Picbuff    dd    0                  ;32 bit scratch pointer to Pic_buff
Tempbuff   dd    0                  ;32 bit scratch pointer reset per blit
Hashbuff   dd    0                  ;32 bit scratch pointer to Hash_buff
CGALStart  dw	 0		    ;Offset from line start to picture start
CGA_Left   dw	 0		    ;Bytes left on this line
CGA_Len	   dw    0		    ;Bytes on line
CGA_Curline dw	 0
CGA_Shift  dw    0		    ;Shift byte right by this much
CGA_Trail  dw    0
CGA_Temp   db    0		    ;Current byte we're working on
CGA_Masks  db    0
	   db    01111111b
	   db    00111111b
	   db    00011111b
	   db    00001111b
	   db    00000111b
	   db    00000011b
	   db    00000001b
CGA_Omasks db    0
	   db    10000000b
	   db    11000000b
	   db    11100000b
	   db    11110000b
	   db    11111000b
	   db    11111100b
	   db    11111110b
BitTable   db    10000000b          ;mask for bit 0 set
           db    01000000b          ;mask for bit 1 set
           db    00100000b          ;and so on
           db    00010000b
           db    00001000b
           db    00000100b
           db    00000010b
           db    00000001b
YTable     dw    200 dup (?)        ;200 lines in each display mode
CoordX0    dw    ?                  ;upper left X coord this call
CoordY0    dw    ?                  ;upper left Y coord this call
CoordX1    dw    ?                  ;lower right X coord this call
CoordY1    dw    ?                  ;lower right Y coord this call
PicWidth   dw    ?                  ;image width in pixels (1 based)
PicHeight  dw    ?                  ;image height in pixels (1 based)
PicLines   dw    ?                  ;number of lines Picbuff will hold
PicLimit   dw    ?                  ;number of bytes to unpack before blit
CurrentY   dw    ?                  ;last referenced Y co-ord during blit
LineCount  dw    ?                  ;line counter during blit operation
TrueWidth  dw    ?                  ;byte width of line with clipping applied
FileHandle dw    ?                  ;file handle in use
InputBuff  dd    ?                  ;input buffer in leiu of file handle
EGACount   dw    ?                  ;number of bytes to blit per line (EGA)
TrueOffset dw    ?                  ;calculated starting byte offset in EGA
TransColor db    ?                  ;transparent color, this call
StartMask  db    ?                  ;bit mask (CGA) for starting bit
PickUnpack db    ?                  ;unpack method (buffer or disk)

;************************** And finally... the code *************************

           .Code

;--------------------------------------------------------------------------
; Trivia:  main entry point for C callable routines.
; Input:   AX         - upper left X of dest region
;          BX         - upper left Y of dest region
;          CX         - lower right X of dest region
;          DX         - lower right X of dest region
;          SI         - picture width (1 based)
;          DI         - picture height (1 based)
;          TransColor - set to transparent color passed in from C
;          PickUnpack - Set to 'BufUnpack' or 'FileUnpack' constant
;                       on BufUnpack :
;                              InputBuff: normalized pointer to data
;                       on FileUîpack :
;                              FileHandle: input file's handle
;--------------------------------------------------------------------------
Trivia     Proc  Near

           Mov   CoordX0,AX         ;set the public access versions of vars
           Mov   CoordY0,BX
           Mov   CoordX1,CX
           Mov   CoordY1,DX
           Mov   PicWidth,SI
           Mov   PicHeight,DI
           Mov   CurrentY,BX        ;init. to top of screen buffer
;--------------------------------------------------------------------------
; Set up the local versions of Pic_buff and Hash_buff
;--------------------------------------------------------------------------
Trivia_0:
           @Mov32 PicBuff,Pic_Buff
           @Mov32 HashBuff,Hash_Buff
           @Normalize PicBuff
           @Normalize Hashbuff
           Call  GrafInit           ;init the graphics variables now
;--------------------------------------------------------------------------
; Initialize the LZ decompression variables
;--------------------------------------------------------------------------
           @Init_Table
           Mov   StackCount,0       ;word count of pushed items
           Mov   BitOffset,0        ;no bits so far
           Mov   ByteCount,0        ;no bytes written yet either
           @Mov32  TempBuff,PicBuff ;working copy of the picbuff
;--------------------------------------------------------------------------
; Initialize the buffer space by reading from input (either buffer or disk)
;--------------------------------------------------------------------------
           Cmp   PickUnpack,FileUnpack ;use files?
           Je    Trivia_F0          ;yup
           Push  DS
           Pop   ES                 ;ES == C's DS
           Lds   SI,InputBuff       ;DS:SI start of input buffer
           Mov   DI,Offset Dgroup:Buffer ;landing area is ES:DI
           Mov   CX,BUFSIZE/2       ;buffer size in words
           Rep   MovSW              ;fill buffer
           Push  SS
           Pop   DS                 ;DS back to 'normal'
           Mov   Word Ptr InputBuff,SI ;update the offset value
           @Normalize InputBuff     ;and normalize it (being consistent...)
           Jmp   Trivia_LZ0   ;jump to the decompress loop
;--------------------------------------------------------------------------
; Load the now un-initialize input buffer via the file handle
;--------------------------------------------------------------------------
Trivia_F0:
           Mov   DX,Offset Dgroup:Buffer ;DS:DX is dest buffer for DOS
           @FRead FileHandle,BUFSIZE     ;load from disk
	   Cmp	 TransColor,0ffh	 ;check for no transparency
	    Je	 Trivia_F1
	    Jmp	 Trivia_LZ0		 ;Has transparency, no speedup
Trivia_F1: Cmp	 Display,USE_VGA
	    Jne	 Trivia_F2		 ;No hair needed for mcga mode
	    Jmp	 Trivia_LZ0
Trivia_F2: Cmp	 Display,USE_CGA
	    Je	 Trivia_CGA		 ;EGA case with no transparency
	   Jmp	 Trivia_LZ0
;--------------------------------------------------------------------------
; Unpack CGA stuff with no transparency
; This is stored 8 bits/byte, with a 1 bit meaning white.  Each line starts
; on a byte boundary.
;--------------------------------------------------------------------------
Trivia_CGA:
	Mov	BX,CurrentY
	Mov	CGA_Curline,BX		;Line we're now on
	Mov	DI,BX
	Shl	DI,1
	Mov	DI,YTable[DI]		;Beginning of first line
	Mov	AX,CoordX0
	Mov	CL,3
	Shr	AX,CL
	Mov	CGALStart,AX
	Add	DI,AX
	Mov	Word Ptr Tempbuff,DI	;Use the graphics segment
	Mov	AX,CoordX0		;First pixel
	And	AX,7			;How many bits to skip in first screen byte
	Mov	CGA_Shift,AX
	Mov	AX,PicWidth
	Mov	CL,3
	Shr	AX,CL	   		;Get width in bytes
	Mov	CGA_Left,AX
	Mov	CGA_Len,AX
	Mov	AX,PicWidth
	And	AX,7			;Number of bits used in last screen byte
	 Je	C_LZ5
	Inc	CGA_Left
	Inc	CGA_Len
C_LZ5:
	Cmp	CGA_Shift,0
	 Jne	C_LZ6
	Mov	CGA_Trail,AX
	Jmp	Short C_LZ4
C_LZ6:	Mov	AX,CoordX0
	Add	AX,PicWidth
	And	AX,7
	Mov	BX,PicWidth
	And	BX,7	   		;Number of bits supplied by last byte
	Cmp	AX,BX
	 Jl	C_LZ7
	Mov	CGA_Trail,AX
	Jmp	Short C_LZ4
C_LZ7:	Neg	AX
	Mov	CGA_Trail,AX
C_LZ4:	Mov	DI,GrafSeg
	Mov	Word Ptr Tempbuff+2,DI
	Mov	CGA_Temp,0
	Cmp	CGA_Shift,0
	 Je	C_LZ0
	Les	DI,Tempbuff
	Mov	AL,ES:[DI]
	Mov	Cl,8
	Sub	CL,CGA_Shift
	Shr	AL,CL
	Shl	AL,CL
	Mov	CGA_Temp,AL		;Initialize Temp
C_LZ0:	@Read_Code			;Get next code
	Cmp	AX,Eof_Token		;done?
	Jne	CNot_EOF		;no, do something with it
;	Call	CGA_Blit		;yes, do last blit
	Ret

CNot_EOF:
	Cmp	AX,Clr_Token		;Clear hash table?
	 Je	CNot_EOF_1
	Jmp	CGA_Process
CNot_EOF_1:
	@Init_Table
	@Read_Code
	Mov	CurCode,AX
	Mov	OldCode,AX
	Mov	K,AL
	Mov	FinalChar,AL
	@CGA_Write
	Jmp	C_LZ0
CGA_Process:
	Mov	CurCode,AX
	Mov	InCode,AX
	Cmp	AX,FreeCode
	Jl	CTabled
	Mov	AX,OldCode
	Mov	CurCode,AX
	Mov	AL,FinalChar
	Push	AX
	Inc	StackCount
CTabled:
	Cmp	CurCode,0ffh
	 Jle	CGA_Char
	Mov	BX,CurCode
	@Index
	Les	DI,Hashbuff
	Add	DI,BX
	Mov	AL,ES:[DI].Char
	Push	AX
	Inc	StackCount
	Mov	AX,ES:[DI].Next
	Mov	CurCode,AX
	Jmp	Short CTabled
CGA_Char:
	Mov	AX,CurCode
	Mov	FinalChar,AL
	Mov	K,AL
	Push	AX
	Inc	StackCount
	Mov	CX,StackCount
CGA_Loop:
	Pop	AX
	Push	CX
	@CGA_Write
	Pop	CX
	Loop	CGA_Frob
	 Jmp	Short CGA_Clear
CGA_Frob:
	Jmp	CGA_Loop
CGA_Clear:
	Mov	StackCount,CX
	@Add_Code
	Mov	AX,InCode
	Mov	OldCode,AX
	Mov	BX,FreeCode
	Cmp	BX,MaxCode
	 Jl	CGA_No_Max
	Cmp	NBits,12
	 Je	CGA_No_Max
	Inc	NBits
	Shl	MaxCode,1
CGA_No_Max:
	Jmp	C_LZ0

;--------------------------------------------------------------------------
; Unpack loop entry point 
;--------------------------------------------------------------------------
Trivia_LZ0:
           @Read_Code               ;get next code from input stream
           Cmp   AX,Eof_Token       ;all done?
           Jne   Not_EOF            ;nope
;--------------------------------------------------------------------------
; We're done, blit what's left and return to C
;--------------------------------------------------------------------------
           Call  Blit               ;finish up with what's left
           Ret                      ;life goes on

Not_EOF:
           Cmp   AX,Clr_Token       ;smash the hash table?
           Je    Not_EOF_1          ;yes sir
           Jmp   Process_Code       ;no, AX is a real compressed code
Not_EOF_1:
           @Init_Table              ;clear hash table
           @Read_Code               ;reload if needed, get next code
           Mov   CurCode,AX         ;current code
           Mov   OldCode,AX         ;same as for now
           Mov   K,AL               ;lo byte
           Mov   FinalChar,AL       ;lo byte
           @Write_Char              ;send the code back
           Jmp   Trivia_LZ0         ;and back for more
Process_Code:
           Mov   CurCode,AX         ;save the latest passed in
           Mov   InCode,AX          ;here too
           Cmp   AX,FreeCode        ;is it in the hash table yet?
           Jl    Tabled_Code        ;sure is
           Mov   AX,OldCode         ;gimmie this back
           Mov   CurCode,AX         ;curcode = oldcode
           Mov   AL,FinalChar       ;reload this too
           Push  AX                 ;save it (here's the stack usage stuff)
           Inc   StackCount         ;one more push has been done
Tabled_Code:
           Cmp   CurCode,0ffh       ;is it a 'char'?
           Jle   Its_A_Char         ;pass out the cigars, it is
           Mov   BX,CurCode         ;need an index so
           @Index                   ;give it
           Les   DI,Hashbuff        ;pointer to base of table
           Add   DI,BX              ;add in base and offset to table
           Mov   AL,ES:[DI].Char    ;grab the suffix
           Push  AX                 ;more of this stuff
           Inc   StackCount         ;stack space dwindles...
           Mov   AX,ES:[DI].Next    ;prefix
           Mov   CurCode,AX         ;our new current code
           Jmp   Short Tabled_Code  ;one more time...
Its_A_Char:
           Mov   AX,CurCode         ;active code
           Mov   FinalChar,AL       ;last char
           Mov   K,AL               ;here too
           Push  AX                 ;stack it up
           Inc   StackCount         ;one last time
           Mov   CX,StackCount      ;ready for the loop
Its_A_Loop:
           Pop   AX                 ;get the next char from TOS
           @Write_Char              ;write it to the output buffer
           Loop  Its_A_Loop         ;until the stack is balanced again
Clear_It:
           Mov   StackCount,CX      ;zero the stackcount
           @Add_Code                ;put code in table
           Mov   AX,InCode          ;in code
           Mov   OldCode,AX         ;is now the oldcode
           Mov   BX,FreeCode        ;next free
           Cmp   BX,MaxCode         ;max yet?
           Jl    No_Max             ;nope
           Cmp   NBits,12           ;run out of bits?
           Je    No_Max             ;stick with 12 'til the clear comes thru
           Inc   NBits              ;next (10,11,12)
           Shl   MaxCode,1          ;give it a bit in both places
No_Max:
           Jmp   Trivia_LZ0         ;back for more

Trivia     Endp

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Blit: Call appropriate graphics blit function, reset 'ByteCount' and
;       Tempbuff values, and return.
;       On exit, DS and CX are preserved.
;--------------------------------------------------------------------------
Blit       Proc  Near

           Cmp   ByteCount,0        ;need to do this?
           Je    Blit_3             ;no need
           Push  DS                 ;save the
           Push  CX                 ;promised registers
           Mov   AX,ByteCount       ;this check is needed in case of
           Cmp   AX,PicLimit        ;a partially full buffer at the end of
           Je    Blit_00            ;the compression loop
           Xor   DX,DX              ;MSW
           Mov   CX,PicWidth        ;bytes in width
           Div   CX                 ;AX = new line count
           Mov   PicLines,AX        ;new line count this buffer
Blit_00:
           @Mov32  Tempbuff,Picbuff ;reset temp pointer to buffer start
           Mov   ES,GrafSeg         ;load this up by default
           Cmp   Display,USE_VGA    ;VGA/MCGA mode?
           Jne   Blit_0             ;no
           Call  Blit_VGA           ;yes
           Jmp   Short Blit_2       ;skip other tests
Blit_0:
           Cmp   Display,USE_EGA    ;EGA mode?
           Jne   Blit_1             ;sorry
           Call  Blit_EGA           ;go for it
           Jmp   Short Blit_2       ;skip to end
Blit_1:
           Cmp   Display,USE_CGA    ;CGA mode?
           Jne   Blit_2             ;forget it, just do cleanup
           Call  Blit_CGA           ;3rd of 3
Blit_2:
           Pop   CX                 ;back from TOS
           Pop   DS                 ;and NOS
           Mov   ByteCount,0        ;we've used up our bytes
Blit_3:
           @Mov32  Tempbuff,Picbuff ;reset the buffer pointer
           Ret                      ;and go unpack more data
Blit       Endp

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Blit_VGA: Fill the screen region defined by (X0,Y0) (X1,Y1) with the
;           image at Picbuff through Picbuff + ByteCount.
;           Apply clipping to the right and bottom parts of the picture.
; On entry, ES points to the video segment for this mode.
;--------------------------------------------------------------------------
Blit_VGA   Proc  Near

           Mov   DX,PicLines        ;reset the line counter for this call
           Mov   BX,CurrentY        ;start at current Y
Blit_V0:
           Cmp   BX,CoordY1         ;'down' out of range?
           Ja    Blit_V5            ;yes, time to go
           Mov   DI,BX              ;need the lookup
           Shl   DI,1               ;Ytable is a word table
           Mov   DI,YTable[DI]      ;got it
           Add   DI,CoordX0         ;add in the base X
           Mov   CX,TrueWidth       ;the 'real' line width displayable
           Mov   AH,TransColor      ;transparent color (0..fe)
           Lds   SI,Tempbuff        ;source
           Cmp   AH,0ffh            ;no mask?
           Je    Blit_V3            ;none: use the quick method
Blit_V1:
           LodSB                    ;AL = pixel value
           Cmp   AL,AH              ;write over screen byte?
           Je    Blit_V2            ;no, AL is "transparent"...
           Mov   ES:[DI],AL         ;put the new pixel in
Blit_V2:
           Inc   DI                 ;next byte in video buffer
           Loop  Blit_V1            ;for 'truewidth' pixels
           Jmp   Short Blit_V4      ;skip the easy move code
Blit_V3:
           Rep   MovSB              ;move those pixels
Blit_V4:
           Push  SS                 ;in small model, SS == C's DS
           Pop   DS                 ;so recover it this way
           Mov   AX,PicWidth        ;width of a buffered line in bytes
           Add   Word Ptr Tempbuff,AX ;skip the entire line width
;           @Normalize TempBuff      ;clean it up
           Inc   BX                 ;next line
           Inc   CurrentY           ;here too
           Dec   DX                 ;one less line to do
           Jne   Blit_V0            ;back for more unless zero to do
Blit_V5:
           Ret                      ;done
Blit_VGA   Endp

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Blit_EGA: Fill the screen region defined by (X0,Y0) (X1,Y1) with the
;           image at Picbuff through Picbuff + ByteCount.
;           Apply clipping to the right and bottom parts of the picture.
; On entry, ES points to the video segment for this mode.
;--------------------------------------------------------------------------
Blit_EGA   Proc  Near
           Cmp   ByteCount,0        ;anything to 'blit'...?
           Je    Blit_E8            ;no, get outta here
           Mov   DX,PicLines        ;reset the line counter for this call
           Mov   BX,CurrentY        ;start at current Y
Blit_E0:
           Cmp   BX,CoordY1         ;'down' out of range?
           Ja    Blit_E8            ;yes, get out quick
	   Cmp	 TransColor,0ffH
	    Je	 EGA_Notr	    ;No transparency, do it faster
           Push  DX                 ;save this (only useful later)
           Push  ES                 ;make DS the video seg
           Pop   DS                 ;and make ES the data seg
           Push  SS                 ;small model
           Pop   ES                 ;there
           Call  CopyEGALine        ;copy line BX from video to buffer
           Call  ApplyEGAData       ;apply the unpacked data to video info
           Call  BlitEGALine        ;and show the new line on the screen
EGA_Next:  Pop   DX                 ;back again
           Inc   BX                 ;next line
           Inc   CurrentY           ;here too
           Dec   DX                 ;one less line to do
           Jne   Blit_E0            ;back for more
Blit_E8:
           @ResetEGA                ;leave the card in normal state
           Ret                      ;done
Blit_EGA   Endp

; For each line, copy the first and last bytes (suitably masked),
; then generate bitplanes in EGAScrath, then do the blt.
EGA_Notr:
	Push	DX
	Push	ES
	Pop	DS
	Push	SS
	Pop	ES		; Set up segment registers funnily
	Push	BX
	Shl	BX,1
	Mov	SI,SS:Ytable[BX]
	Mov	DI,Offset Dgroup:EGAScratch
	Mov	BX,SS:EGACount
	Dec	BX			;Since we're not doing a rep movsb
	Mov	DX,SS:TrueOffset
	Add	SI,DX
	Add	DI,DX
	Xor	AH,AH
EGA_Notr_0:
	@ReadPlane
	Mov	AL,[SI]
	Mov	ES:[DI],AL
	Add	DI,BX			;Move to the end
	Add	SI,BX
	Mov	AL,[SI]
	Mov	ES:[DI],AL
	Add	DI,80
	Sub	DI,BX
	Sub	SI,BX
	Inc	AH
	Cmp	AH,4
	 Jne	EGA_Notr_0
	@ResetEGA
	Pop	BX
;Now have the beginning and end of the line copied from the EGA card.
;Turn the pixels into bitplanes, then do the transfer.
	Push	DS
	Mov	CX,SS:TrueWidth
	Mov	DI,Offset Dgroup:EGAScratch
	Add	DI,SS:TrueOffset
	Mov	DL,SS:StartMask
	Lds	SI,SS:Tempbuff
EGA_Notr_1:
	LodSB
	@SetALPixel
	Shr	DL,1
	 Jnc	EGA_Notr_2
	Mov	DL,80h
	Inc	DI
EGA_Notr_2:
	Loop	EGA_Notr_1
	Push	SS
	Pop	DS
	Mov	AX,PicWidth
	Add	Word Ptr Tempbuff,AX
;	 @Normalize TempBuff
	Pop	DS
	Call	BlitEGALine
	Jmp	EGA_Next

;--------------------------------------------------------------------------
; CopyEGALine: read line BX from the video card and copy it into
;              EGAScratch so that the plane data aligns with the
;              +0,+80,+160,+240 offsets into the buffer.
; On entry, DS video segment, ES is C's DS, BX is line number.
; On exit,  DS, ES, and BX must be preserved.
;--------------------------------------------------------------------------
           Even
CopyEGALine  Proc Near

           Push  BX                 ;save me
           Shl   BX,1               ;*2
           Mov   SI,SS:YTable[BX]   ;starting position
           Mov   DI,Offset Dgroup:EGAScratch ;base internal line buffer
           Mov   BX,SS:EGACount     ;line width for looping
           Mov   DX,SS:TrueOffset   ;starting offsets from lines
           Add   SI,DX              ;add in the offset
           Add   DI,DX              ;for both buffers
           Xor   AH,AH              ;start with plane zero
CopyEGALine_0:
           @ReadPlane               ;read plane AH (0..3)
           Mov   AL,[SI]            ;latch in new state
           Mov   CX,BX              ;number of bytes to move
           Rep   MovSB              ;put it in the buffer
           Add   DI,80              ;skip to next plane
           Sub   DI,BX              ;backup for additional moves
           Sub   SI,BX              ;yea yea, here too
           Inc   AH                 ;next plane
           Cmp   AH,4               ;done yet?
           Jne   CopyEGALine_0      ;yes
           @ResetEGA                ;no, put the card in a nice state
           Pop   BX                 ;in the right order
           Ret                      ;see ya
CopyEGALine Endp

;--------------------------------------------------------------------------
; ApplyEGAData: Take from the TempBuff and apply unpacked bytes to the
;               bit plane formatted data in EGAScratch.
; On entry, DS = video seg, ES = C's DS, BX = current line
; On exit,  DS, ES, BX must be preserved
;--------------------------------------------------------------------------
           Even
ApplyEGAData  Proc Near
           Push  DS                 ;save me
           Mov   CX,SS:TrueWidth    ;the 'real' line width displayable
           Mov   AH,SS:TransColor   ;transparent color (0..fe)
           Mov   DI,Offset Dgroup:EGAScratch ;dest
           Add   DI,SS:TrueOffset   ;adjusted for byte position
           Mov   DL,SS:StartMask    ;starting bit mask value
           Lds   SI,SS:Tempbuff     ;source
	   cmp	 AH,0ffh	    ; are we doing anything transparent?
ApplyEGA_0:
           LodSB                    ;input byte (only lo nibble counts)
           Cmp   AL,AH              ;did we match?
           Je    ApplyEGA_2         ;yes, skip this here pixel
ApplyEGA_1:
           @SetALPixel              ;keep things simple this way
ApplyEGA_2:
           Shr   DL,1               ;slide mask down the bits
           Jnc   ApplyEGA_3         ;no drop off yet
           Mov   DL,80h             ;up to top of next byte
           Inc   DI                 ;and skip to next byte in buffer
ApplyEGA_3:
           Loop  ApplyEGA_0         ;for CX pixels in the buffer
	   Push  SS
	   Pop   DS
	   Mov	 AX,PicWidth
           Add   Word Ptr Tempbuff,AX ;skip the entire line width
;           @Normalize TempBuff      ;clean it up
           Pop   DS                 ;restore video seg as DS (ES never changed)
           Ret                      ;and go back
ApplyEGAData   Endp


;--------------------------------------------------------------------------
; BlitEGALine: Move planed data from EGAScratch onto screen at Y line BX.
; On entry, DS = video segment, ES = C's data segment, BX = dest Y
; On exit,  DS = C's data segment, ES = video segment, BX preserved
;--------------------------------------------------------------------------
           Even
BlitEGALine  Proc Near

           Push  BX                 ;save for return to caller
           Push  DS                 ;need to swap these here segment registers
           Push  ES                 ;this should be familiar
           Pop   DS                 ;by now
           Pop   ES                 ;xchg ds,es
           Mov   DI,BX              ;dest Y
           Shl   DI,1               ;for lookup in table
           Mov   DI,YTable[DI]      ;base Y
           Mov   SI,Offset Dgroup:EGAScratch ;start of data (p0,p1,p2,p3)
           Mov   BX,TrueOffset      ;keep this around
           Add   DI,BX              ;offset to starting X byte
           Add   SI,BX              ;compensate here too
           Mov   BX,EGACount        ;number of bytes to move
           Mov   AH,1               ;write plane selector
EGALine_0:
           @WritePlane              ;enable the card
           Mov   AL,ES:[DI]         ;latch the new state in
           Mov   CX,BX              ;# of bytes to move
           Rep   MovSB              ;move data onto screen
           Shl   AH,1               ;next plane
           Add   SI,80              ;add in bytes per line; skip to next plane
           Sub   SI,BX              ;compensate for moved data
           Sub   DI,BX              ;one more
           Cmp   AH,10h             ;video seg hit end yet?
           Jne   EGALine_0          ;not yet
           Pop   BX                 ;get BX back from TOS
           Ret                      ;and go back
BlitEGALine EndP

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Blit_CGA: Fill the screen region defined by (X0,Y0) (X1,Y1) with the
;           image at Picbuff through Picbuff + ByteCount.
;           Apply clipping to the right and bottom parts of the picture.
; On entry, ES points to the video segment for this mode.
;--------------------------------------------------------------------------
Blit_CGA   Proc  Near
           Mov   DX,PicLines        ;reset the line counter for this call
           Mov   BX,CurrentY        ;start at current Y
Blit_C0:
           Cmp   BX,CoordY1         ;'down' out of range?
           Jna   Blit_C0x	    ;nope, doing just fine
           Jmp	 Blit_C6	    ;yes, time to go
Blit_C0x:
           Push  DX                 ;save line counter at TOS
           Mov   DI,BX              ;need the lookup
           Shl   DI,1               ;Ytable is a word table
           Mov   DI,YTable[DI]      ;got it
           Mov   AX,CoordX0         ;starting X
           Mov   CL,3               ;divide by 8
           Shr   AX,CL              ;to find starting byte (bit per pixel)
           Add   DI,AX              ;mix it in
           Mov   CX,TrueWidth       ;the 'real' line width displayable
           Mov   AH,TransColor      ;transparent 'bit'
           Mov   DL,StartMask       ;starting mask (single bit)
           Lds   SI,Tempbuff        ;source
           Cmp   AH,0ffh            ;transparent off?
           Je    Blit_C1nt	    ; not transparent color, so don't look
Blit_C1:
           LodSB                    ;AL = pixel value (0 or 1)
           Cmp   AL,AH              ;write over screen byte?
           Je    Blit_C4            ;no, AL is "transparent"...
Blit_C2:
           And    AL,1              ;zero or one?
           Je   Blit_C3             ;set it (one)
           Not   DL                 ;invert the mask to all but one bit set
           And   ES:[DI],DL         ;and clear that one 0 bit
           Not   DL                 ;put it back to normal
           Jmp   Short Blit_C4      ;do pointer and mask stuff now
Blit_C3:
           Or    ES:[DI],DL         ;set the pixel, don't touch others
Blit_C4:
           Shr   DL,1               ;shift the mask bit right
           Jnc   Blit_C5            ;no bits fell off, don't need new mask yet
           Mov   DL,80h             ;hi bit set
           Inc   DI                 ;and next screen byte
Blit_C5:
           Loop  Blit_C1            ;for 'truewidth' pixels
	   Jmp	 short Blit_C6x	    ; done
;
; Special loop if no transparent color
;
Blit_C1nt:
           LodSB                    ;AL = pixel value (0 or 1)
           And   AL,1               ;zero or one?
           Je    Blit_C2nt          ;set it (one)
           Not   DL                 ;invert the mask to all but one bit set
           And   ES:[DI],DL         ;and clear that one 0 bit
           Not   DL                 ;put it back to normal
           Jmp   Short Blit_C3nt    ;do pointer and mask stuff now
Blit_C2nt:
           Or    ES:[DI],DL         ;set the pixel, don't touch others
Blit_C3nt:
           Shr   DL,1               ;shift the mask bit right
           Jnc   Blit_C4nt          ;no bits fell off, don't need new mask yet
           Mov   DL,80h             ;hi bit set
           Inc   DI                 ;and next screen byte
Blit_C4nt:
           Loop  Blit_C1            ;for 'truewidth' pixels
;
; Okay, go back to normal
;
Blit_C6x:
           Push  SS                 ;in small model, SS == C's DS
           Pop   DS                 ;so recover it this way
           Mov   AX,PicWidth        ;width of a buffered line in bytes
           Add   Word Ptr TempBuff,AX ;skip the entire line width
;           @Normalize TempBuff      ;clean it up
           Inc   BX                 ;next line
           Inc   CurrentY           ;here too
           Pop   DX                 ;recover line count from TOS
           Dec   DX                 ;one less line to do
           Je    Blit_C6	    ;no more left
	   Jmp	 Blit_C0            ;back for more unless zero to do
Blit_C6:
           Ret                      ;done
Blit_CGA   EndP
;..........................................................................

           Even
;--------------------------------------------------------------------------
; GrafInit: Initialize Trivia variables for the indicated graphics mode.
;           In this version of the graphics driver, all input comes in as
;           single byte pixel values irregardless of graphics data type.
;           Therefore the calculations for widths and byte counts in the
;           various buffers can all be done here instead of by each
;           initializing function.
; On exit, DS is assumed to have been preserved.
;--------------------------------------------------------------------------
GrafInit   Proc  Near
;--------------------------------------------------------------------------
; Calc. number of lines Picbuff will hold of this picture.  Since the
; standard in use are unpacked bytes, one per pixel:
;          PicLines = (Pic_buff_size * 16) / PicWidth
;--------------------------------------------------------------------------
           Mov   AX,Pic_buff_size   ;size of Picbuff in paragraphs
           Mov   CL,4               ;2^4 = 16
           Shl   AX,CL              ;*16 convert to bytes
           Xor   DX,DX              ;MSW of 32 bit register
           Mov   CX,PicWidth        ;width (1 based)
           Div   CX                 ;AX = result; DX = remainder (ignored)
           Mov   PicLines,AX        ;save result
;--------------------------------------------------------------------------
; Calc. the number of bytes to unpack before performing the graphics blit:
;          PicLimit = PicLines * PicWidth
;--------------------------------------------------------------------------
           Xor   DX,DX              ;clear MSW again (CX is still PicWidth)
           Mul   CX                 ;AX = PicLines * PicWidth
           Mov   PicLimit,AX        ;since byte per pixel, stop here
;--------------------------------------------------------------------------
; Find the 'true width' of the display buffer, or the number of pixels
; (bytes) that will fit on one clipped line.
;--------------------------------------------------------------------------
;           Mov   AX,CoordX1         ;right edge
;           Sub   AX,CoordX0         ;find port width (NOT picture width)
           Mov   AX,PicWidth
           Mov   TrueWidth,AX       ;number of bytes (pixels) to show in port
;--------------------------------------------------------------------------
; Now setup for the Y table initializations
;--------------------------------------------------------------------------
           Push  DS                 ;also known as
           Pop   ES                 ;a mov es,ds
           Mov   DI,Offset Dgroup:YTable ;common entry condition
           Mov   CX,200             ;# of lines in each graphics mode
           Cmp   Display,USE_CGA    ;CGA 640 x 200 2 color?
           Je    Init_CGA           ;yes
GrafInit_0:
           Cmp   Display,USE_EGA    ;EGA 640 x 200 16 color?
           Je    Init_EGA           ;yes
GrafInit_1:
           Cmp   Display,USE_VGA    ;MCGA/VGA 320 x 200 256 color?
           Je    Init_VGA           ;3rd of 3 possible
           Ret                      ;sorry, nothing matched

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Init_CGA: Initialize Trivia variables for the CGA routines
;           On entry, ES:DI points to YTable, CX = 200
;           On exit,  all non-segment registers trashed
;--------------------------------------------------------------------------
Init_CGA:
           Mov   BX,CoordX0         ;find starting bit mask
           And   BX,7               ;by taking X mod 8 as an index
           Mov   AL,BitTable[BX]    ;into the bit table
           Mov   StartMask,AL       ;put it here for Blit_CGA to use
           Cmp   GrafSeg,0          ;need to do more?
           Jne   Init_CGA_2         ;no
           Mov   GrafSeg,SEG_CGA    ;install the segment pointer
           Xor   BX,BX              ;this will count lines 0..199
Init_CGA_1:
           Mov   DX,BX              ;current line number
           Mov   DH,DL              ;multiply by 256 (8 shifts left)
           And   DX,1feh            ;force into range (l shifted with clr 0b)
           Shl   DX,1               ;*512
           Shl   DX,1               ;*1024
           Shl   DX,1               ;*2048
           Mov   AX,DX              ;working copy
           And   AH,7               ;gimmie lower 3 bits
           Shl   DX,1               ;still higher
           Shl   DX,1               ;last one (promise)
           Add   AX,DX              ;we've got interlace and offset now
           StoSW                    ;save the crazy result
           Inc   BX                 ;next line 0..199
           Loop  Init_CGA_1         ;that's 200 lines
Init_CGA_2:
           Ret

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Init_VGA: Initialize Trivia variables for the VGA routines
;           On entry, ES:DI points to YTable, CX = 200
;           On exit,  all non-segment registers trashed
;--------------------------------------------------------------------------
Init_VGA:
           Cmp   GrafSeg,0          ;already done the init?
           Jne   Init_VGA_1         ;sure have
           Mov   GrafSeg,SEG_VGA    ;install the segment pointer
           Xor   AX,AX              ;initialize Y buffer offset to zero
Init_VGA_0:
           StoSW                    ;save the Y offset for this line
           Add   AX,320             ;320 bytes per line (byte per pixel) linear
           Loop  Init_VGA_0         ;for 200 lines (200 downto 1)
Init_VGA_1:
           Ret                      ;done with init

;..........................................................................

           Even
;--------------------------------------------------------------------------
; Init_EGA: Initialize Trivia variables for the EGA routines
;           On entry, ES:DI points to YTable, CX = 200
;           On exit,  all non-segment registers trashed
;--------------------------------------------------------------------------
Init_EGA:
           Push  CX                 ;save the 200 entry condition
           Mov   BX,CoordX0         ;find starting bit mask
           And   BX,7               ;by taking X mod 8 as an index
           Mov   AL,BitTable[BX]    ;into the bit table
           Mov   StartMask,AL       ;put it here for Blit_CGA to use
;--------------------------------------------------------------------------
; Find the value for TrueOffset and EGACount, where
;          EGACount   - number of bytes in line segment to move
;          TrueOffset - The 'real' starting byte offset (0..79) from the
;                       left edge of the Y line being processed.
;--------------------------------------------------------------------------
           Mov   CL,3               ;setup for div 8 operations
           Mov   AX,CoordX0         ;starting X
	   Mov	 DX,AX
	   Add	 DX,TrueWidth
	   Dec	 DX
;           Mov   DX,CoordX1         ;ending X
           Shr   AX,CL              ;convert to byte offset
           Shr   DX,CL              ;same here
           Sub   DX,AX              ;line width
           Inc   DX                 ;plus one for CX looping value
           Mov   EGACount,DX        ;here's the width, ra ra ra
           Mov   TrueOffset,AX      ;starting byte offset X0
Init_EGA_0:
           Pop   CX                 ;restore entry register
           Cmp   GrafSeg,0          ;need this other init too?
           Jne   Init_EGA_2         ;yup
           Mov   AX,SEG_EGA         ;video segment
           Mov   GrafSeg,AX         ;install the segment pointer
           Xor   AX,AX              ;initialize Y buffer offset to zero
Init_EGA_1:
           StoSW                    ;save the Y offset for this line
           Add   AX,640/8           ;80 bytes per line linear mapping
           Loop  Init_EGA_1         ;for 200 lines (200 downto 1)
Init_EGA_2:
           Ret                      ;all for now

GrafInit   EndP


;--------------------------------------------------------------------------

;=============================================================================

           @FUNC disp_pic_f,<X0:Word,Y0:Word,W:Word,H:Word,X1:Word,Y1:Word,HANDLE:Word,TRANS:Byte>
           @LINK SI,DI,DS
           Mov   PickUnpack,FileUnpack    ;tell LZ input is from a file
           Mov   AL,TRANS           ;transparent color
           Mov   TransColor,AL      ;pass to Trivia via data segment
           Mov   AX,HANDLE          ;open file's handle
           Mov   FileHandle,AX      ;make it 'global'
           @StandardEntry           ;load registers for standard entry to Trivia
           Call  Trivia             ;go for it
           @UNLINK                  ;sure, it looks easy
disp_pic_f EndP

;=============================================================================

           @FUNC disp_pic_b,<X0:Word,Y0:Word,W:Word,H:Word,X1:Word,Y1:Word,BUF_OFS:Word,BUF_SEG:Word,TRANS:Byte>
           @LINK SI,DI,DS
           Mov   PickUnpack,BufUnpack     ;tell LZ input is from a buffer
           Mov   AL,TRANS           ;transparent color
           Mov   TransColor,AL      ;pass to Trivia via data segment
           Mov   AX,BUF_OFS         ;32 bit parms don't work so well 
           Mov   Word Ptr InputBuff,AX
           Mov   AX,BUF_SEG         ;so it's back to word kludges for now
           Mov   Word Ptr InputBuff+2,AX
           @Normalize InputBuff     ;normalize the pointer for our consumption
           @StandardEntry           ;setup
	   Cld			    ; make sure direction flag is positive
           Call  Trivia             ;work work wrok
           @UNLINK                  ;see ya
disp_pic_b EndP

           End

;----------------------------------------------------------------------------

