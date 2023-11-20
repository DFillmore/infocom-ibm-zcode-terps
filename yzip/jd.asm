;
; Jonathan Arnold's asm tries
;
        DOSSEG                  ; select Intel-contvention segment ordering
        .MODEL Small            ; Small C stuff

        .DATA                   ; TC-compatible init data segment
        EXTRN _Scrx, _Scry      ; Cursor X,Y pos
        EXTRN _Fgcolor, _Bgcolor ; Foreground/Background color of char
        EXTRN _Font             ; pointer to font data
	EXTRN _Win_hlmode	; Current highlight mode, for underlining
	EXTRN _Font_height	; Font height (no kidding)

        .DATA?                  ; TC-compatible un-init data segment
NextLine  DW    ?               ; offset to next line

        .CODE                   ; TC-compatible code segment
BitsPerLine    EQU     320      ; how many pixels in one line
UNDFLG	EQU	4		; bit in hlmode of underlining

;
; display_char13( character, cw, monoflag )
;       int character - character to output
;       int cw - character width
;	int monoflag - 0 if not monospaced, width of mono font if it is
;
;  Just print a character at position Scrx, Scry
;
; Courtesy of "Programmer's Guide to PC & PS/2 Video Systems", Richard Wilton
;       MicroSoft Press, pp. 286-288
;
        PUBLIC	_display_char13
_display_char13 PROC NEAR
        ARG   Character:WORD,Cw:WORD,MonoFlag:WORD
	LOCAL	Hlmode:WORD=AUTO_SIZE

        push    bp              ; preserve a few registers
        mov     bp,sp
        sub     sp,AUTO_SIZE    ; reserve some room for local vars
        push    di
        push    si
        push    ds

	mov	ax,_Win_hlmode	; save for later check
	mov	[Hlmode],ax
;
; calculate first pixel address
;
        mov     ax,_Scry        ; get x,y pos
        mov     bx,_Scrx
        call    PixelAddr13     ; ES:BX point to start of buffer
        mov     di,bx           ; make it be ES:DI
;
; set up character definition table addressing
;
        mov     cx,8            ; CX = POINTS (pixel rows in chars)

        mov     ax,Word Ptr _Font+2 ; Get segment and offset
        push    ax              ; save a minute
        mov     ax,Word Ptr _Font   ; this is the offset
        mov     si,ax           ; put offset into si
        pop     ds              ; DS:SI - start of char table

        xor     ax,ax           ; magical clear
        mov     ax,[Character]  ; Character we would like to print
        mul     cl              ; AX - offset into char def table
        add     si,ax           ; SI - addr of char def
;
; figger offset to next line
;
        mov     ax,BitsPerLine  ; how many bits in one line
	cmp	[MonoFlag],0	; are we doing monospaced font?
	je	L01		; no
	sub	ax,[MonoFlag]	; then subtract mono spaced width
	jmp	short L02	; and done
L01:
        sub     ax,[Cw]         ; - width of char
L02:
        mov     NextLine,ax     ; = offset to next pixel line
;
; store the character in the video buffer
;
        mov     bl,_Fgcolor     ; BL - foreground pixel value
        mov     bh,_Bgcolor     ; BH - background pixel value

L10:    push    cx              ; preserve CX across loop
;
; check for underlining next to last line
;
	cmp	cx,1		; last line?
	jne	short LuM	; nope
	test	[Hlmode],UNDFLG ; underlining on?
	jz	short LuM	 ; nope
	mov	al,0FFh		; turn on all bits for underline
        mov     cx,[Cw]         ; CX - char width in pixels
	cmp	[MonoFlag],0	; monofont?
	je	short Ljd	; nope, so use this font
	mov	cx,[MonoFlag]	; use monofont width
	jmp	short Ljd	; done
LuM:
        lodsb   		; go get the char data
        mov     cx,[Cw]         ; CX - char width in pixels
;
; now, mess around with data if mono_spaced font
;
	cmp	[MonoFlag],0	; is it zero?
	je	Ljd		; yes, so no playing with it
	cmp	[MonoFlag],cx	; how does width look to it
	je	Ljd		; no messing if same width
	mov	ah,cl		; pick up real width
	mov	cx,[MonoFlag]	; set width to be mono width
	ja	Ljd1		; mono width is wider than normal width
;
; width is wider than mono width, so cut out 1/2 of extra
;
	sub	ah,cl		; see how much is left
	shr	ah,1		; 1/2 for extra at beginning
	mov	cl,ah		; get shifting amount
	shl	al,cl		; and move data to the left
	jmp	short Ljd2	; done
;
; Width is less than mono width, so try and center data
;
Ljd1:
	sub	cl,ah		; get how much wider is mono font
	shr	cl,1		; /2 to get amount at beginning
	jnc	Ljd10		; okay, no wrap
	inc	cl		; always move at least one
Ljd10:
	shr	al,cl		; and try to center data
Ljd2:
	mov	cx,[MonoFlag]	; set width to be mono width
Ljd:
        mov     ah,al           ; AH - bit pattern for next pixel row

L11:    mov     al,bh           ; let's assume background color more likely
        shl     ah,1            ; check pixel
        jnc     L12             ; okay, it is a background color
        mov     al,bl           ; oops, really foreground

L12:    stosb                   ; send out the pixel
        loop    L11             ; and check next one

        add     di,NextLine     ; point to next row of pixels
        pop     cx              ; get counter back
        loop    L10             ; go do next line

        pop     ds              ; get regs back
        pop     si
        pop     di
        mov     sp,bp
        pop     bp
        ret

_display_char13 ENDP

;
; PixelAddr13
;       Determine buffer addres of pixel in MCGA mode 13
;
; Inputs:
;       AX = y coordinate (0-199)
;       BX = x coordinate (0-319)
;
; Returns:
;       BX = byte offset in buffer
;       ES = video buffer segment
;
OriginOffset EQU 0
VideoBufferSeg EQU 0A000h

PixelAddr13 PROC NEAR
        xchg    ah,al           ; AX = 256*y
        add     bx,ax           ; BX = 256*y + x
        shr     ax,1
        shr     ax,1            ; AX = 64*y
        add     bx,ax           ; BX = 320*y + x
        add     bx,OriginOffset ; BX = byte offset into buffer
        mov     ax,VideoBufferSeg
        mov     es,ax           ; ES:BX = byte address of pixel
        ret
PixelAddr13 ENDP

;
; DisplayChar10( character ) - display a character in EGA Mode 0x0E
;       int character - character to output
;  Just print a character at position Scrx, Scry
;
; Courtesy of "Programmer's Guide to PC & PS/2 Video Systems", Richard Wilton
;       MicroSoft Press, pp. 28288-292
;
BytesPerLine    =       80

        PUBLIC _display_char10
_display_char10  PROC NEAR
        ARG     Character:WORD
        LOCAL   VARshift:WORD,Fore:BYTE,Back:BYTE,Hlmode:WORD=AUTO_SIZE

        push    bp              ; preserve a few registers
        mov     bp,sp
        sub     sp,AUTO_SIZE    ; reserve some room for local vars
        push    di
        push    si
        push    ds

	mov	ax,_Win_hlmode	; save for later check
	mov	[Hlmode],ax

        mov     al,_Bgcolor     ; set back/foreground color
        mov     Back,al
        mov     al,_Fgcolor     ; we did
        mov     Fore,al
;
; calculate first pixel address
;
        mov     ax,_Scry        ; get x,y pos
        mov     bx,_Scrx
        call    PixelAddr10     ; ES:BX point to start of buffer
        mov     di,bx           ; make it be ES:DI

        inc     cx
        and     cl,7            ; CL == # bits to shift to mask char

        mov     ch,0FFh
        shl     ch,cl           ; CH == bit mask for right side of char
        mov     [VARshift],cx

        push    es              ; prevserve video buffer segment
        mov     si,bx           ; SI == video buffer offset
;
; set up char def table addressing
;
        mov     ax,40h
        mov     ds,ax           ; DS = segment of BIOS Video Display area
        mov     cx,ds:[85h]     ; CX = POINTS (rows in characters)

        xor     ax,ax           ; magical clear
        mov     ds,ax           ; DS = absolute zero

        mov     ax,[Character]  ; AL = character code
        mov     bx,43h*4        ; DS:BX - int 43 vector
        les     di,ds:[bx]      ; ES:DI - start of char table
        mul     cl              ; AX = offset into char table
        add     di,ax           ; DI = addr of char data
        pop     ds              ; DS:SI - video buffer
;
; set up Graphics Controller regs
;
        mov     dx,3CEh         ; Graphics Controller address reg port
        mov     ax,0A05h        ; AL = Mode register number
                                ; AH = Write mode 2 (bits 0-1)
                                ;       Read mode 1 (bit 4)
        out     dx,ax
        mov     ah,0            ; Read-Modify-Write bits (just move)
        mov     al,3            ; Data Rotate/Function Select reg
        out     dx,ax

        mov     ax,0007         ; AH = Color Don't Care bits
                                ; AL = Color Don't Care reg number
        out     dx,ax           ; "don't care" for all bit planes
;
; select output routine depnding on whether character is byte aligned
;
        mov     bl,Fore         ; BL = foreground color
        mov     bh,Back         ; BH = background color
        cmp     byte ptr [VARshift],0   ; test # bits to shift
        jne     L20             ; jump if character is not byte-aligned
;
; routine for byte aligned characters
;
        mov     al,8            ; AL = bit mask register number

L30:
;
; check for underlining next to last line
;
	cmp	cx,1		; last line?
	jne	short LuE	; nope
	test	[Hlmode],UNDFLG ; underlining on?
	jz	short LuE	 ; nope
	mov	ah,0FFh		; turn on all bits for underline
	jmp	short LdE	; done
LuE:
	mov     ah,es:[di]      ; AH = pattern for next row of pixels
LdE:
        out     dx,ax           ; update bit mask registre
        and     [si],bl         ; update foreground pixels

        not     ah
        out     dx,ax
        and     [si],bh         ; update background pixels

        inc     di              ; ES:DI - next byte in char def table
        add     si,BytesPerLine ; increment to next line in video buffer
        loop    L30          
        jmp     short Lexit
;
; routine for non-byte aligned characters
;
L20:    push    cx              ; preserve loop counter
;
; check for underlining next to last line
;
	cmp	cx,1		; last line?
	jne	short LuE1	; nope
	test	[Hlmode],UNDFLG ; underlining on?
	jz	short LuE1	 ; nope
	mov	al,0FFh		; turn on all bits for underline
	jmp	short LdE1	; done
LuE1:
;
; left side of character
;       
        mov     al,es:[di]      ; AL = bits for next row of pixels
LdE1:
        mov     cx,[VARshift]   ; CH = mask for left side of character
                                ; CL = # bits to shift left
        xor     ah,ah
        shl     ax,cl           ; AH = bits for left side of char
                                ; AL = bits for right side of char
        push    ax              ; save right side
        mov     al,8            ; Bit Mask Register number
        out     dx,ax           ; set bit mask for foreground pixels
        and     [si],bl         ; update foreground pixels

        not     ch              ; CH = mask for left side of char
        xor     ah,ch           ; AH = bits for background pixels
        out     dx,ax           ; set bit mask
        and     [si],bh         ; update background pixels
;
; right side of character
;
        pop     ax
        mov     ah,al           ; AH = bits for right side of char
        mov     al,8
        out     dx,ax           ; set bit mask

        inc     si              ; DS:SI - right side of char in buffer
        and     [si],bl         ; update foreground pixels

        not     ch              ; CH = mask for right side of char
        xor     ah,ch           ; AH = bits for background pixels
        out     dx,ax           ; set bit mask
        and     [si],bh         ; update background pixels
;
; increment to next row of pixels in character
;
        inc     di              ; ES:DI - next byte in char def table
        dec     si
        add     si,BytesPerLine ; DS:SI - next line in video buffer

        pop     cx
        loop    L20
;
; restore default Graphics Controller registers
;
Lexit:  mov     ax,0FF08h       ; default Bit Mask
        out     dx,ax

        mov     ax,0005         ; default Mode register
        out     dx,ax

        mov     ax,0003         ; default Data Rotate/Function Select
        out     dx,ax

        mov     ax,0FC7h        ; deffault Color Don't Care
        out     dx,ax

        pop     ds              ; get regs back
        pop     si
        pop     di
        mov     sp,bp
        pop     bp
        ret

_display_char10 ENDP

;
; PixelAddr10
;       Determine buffer address of pixel in Mode 0x0E
;
; Inputs:
;       AX = y coordinate (0-199)
;       BX = x coordinate (0-640)
;
; Returns:
;       AH = bit mask
;       BX = byte offset in buffer
;       CL = nubmer of bits to shift left
;       ES = video buffer segment
;
PixelAddr10 PROC NEAR
        mov     cl,bl           ; CL = low-order byte of x
        push    dx              ; preserve DX

        mov     dx,BytesPerLine ; AX = y * BytesPerLine
        mul     dx

        pop     dx
        shr     bx,1
        shr     bx,1
        shr     bx,1            ; BX = x/8
        add     bx,ax           ; BX = y*BytesPerLine + x/8
        add     bx,OriginOffset ; BX = byte offset in video buffer

        mov     ax,VideoBufferSeg
        mov     es,ax           ; ES:BX = byte address of pixel

        and     cl,7            ; CL = x & 7
        xor     cl,7            ; CL = nubmer of bits to shift left
        mov     ah,1            ; AH = unshifted bit mask
        ret
PixelAddr10 ENDP
;
; DisplayChar06( character ) - display a character in CGA Mode 6
;       int character - character to output
;  Just print a character at position Scrx, Scry with Foreground of
; Fgcolor and a background of Bgcolor
;
; Courtesy of "Programmer's Guide to PC & PS/2 Video Systems", Richard Wilton
;       MicroSoft Press, pp. 276-278
;
        PUBLIC _display_char06
_display_char06  PROC NEAR
        ARG     Character:WORD
        LOCAL   VARmask:WORD,VARtoggle:BYTE,Hlmode:WORD=AUTO_SIZE
        push    bp              ; preserve a few registers
        mov     bp,sp
        sub     sp,AUTO_SIZE    ; reserve some room for local vars
        push    di
        push    si
        push    ds

	mov	ax,_Win_hlmode	; save for later check
	mov	[Hlmode],ax
;
; set up foreground pixel toggle mask
;
	mov	ah,_Fgcolor	; AH = 0 or 1 (foreground pixel value)
	ror	ah,1		; high-order bit of ah = 0 or 1
	cwd			; propagate high-order bit through DX
	not	dx		; DX =    0 if foreground = 1
				;      FFFF if foreground = 0
	mov	VARtoggle,dx
;
; calculate first pixel address
;
	mov	ax,_Scry	; AX = y
	mov	bx,_Scrx	; BX = x
	call	PixelAddr06	; ES:BX -> buffer
				; CL = # bits to shift left
	xor	cl,7		; CL = # bits to rotate right
	mov	ax,0FF00h
	ror	ax,cl		; AX = bit mask in proper position
	mov	VARmask,ax
;
; set up video buffer addressing
;
	mov	dx,2000h	; increment for video buffer interleave
	mov	di,80-2000h	; increment from last to first interleave
	test	bx,2000h	; set sero flag if BX in 1st interleave
	jz	CGA01
	xchg	di,dx		; exchange increment values if 1st pixel
				;  lies in 1st interleave
;
; set up character definition table addressing
;
CGA01:	push	bx		; preserve buffer address
	mov	ch,_Font_height	; CH = POINTS (pixel rows in char)
	mov	ax,0F000h	; point to character generator data
	mov	ds,ax		; DS = absolute zero
	mov	ax,[Character]	; AX = character code
CGA02:	mov	si,0FA6Eh	; DS:SI -> start of table
	mul	ch		; AX = offset into char def table
	add	si,ax		; SI = addr of char definition
	pop	bx		; restore buffer address
	test	cl,cl		; test # bits to rotate
	jnz	CGA20		; jump if character is not byte-aligned
;
; routine for byte aligned characters
;
	mov	ah,VARtoggle	; AH = foreground toggle mask
	xchg	ch,cl		; CX = points

CGA10:
	lodsb			; AL = bit pattern for next pixel row
;
; check for underlining next to last line
;
	cmp	cx,1		; last line?
	jne	short LdC	; nope
	test	[Hlmode],UNDFLG ; underlining on?
	jz	short LdC	 ; nope
	mov	al,0FFh		; turn on all bits for underline
LdC:
	xor	al,ah		; toggle pixels if foreground = 0
	mov	es:[bx],al	; store pixels in buffer
	add	bx,dx		; bx = next row in buffer
	xchg	di,dx		; swap buffer increments
	loop	CGA10
	jmp	short CGAexit
;
; routine for non-byte-aligned characters
;
CGA20:	mov	ax,VARmask
	and	es:[bx],ax	; mask character pixels in buffer
	xor	ah,ah
	lodsb			; AX = bit pattern for next pixel row
;
; check for underlining next to last line
;
	cmp	ch,1		; next to last line?
	jne	short LuC1	; nope
	test	[Hlmode],UNDFLG ; underlining on?
	jz	short LuC1	 ; nope
	mov	al,0FFh		; turn on all bits for underline
	jmp	short LdC1	; done
LuC1:
LdC1:
	xor	al,VARtoggle	; toggle pixels if foreground = 0
	ror	ax,cl		; rotate pixels into position
	or	es:[bx],ax	; store pixels in buffer
	add	bx,dx		; BX = next row in buffer
	xchg	di,dx		; swap buffer increments
	dec	ch
	jnz	CGA20

CGAexit: pop     ds              ; get regs back
        pop     si
        pop     di
        mov     sp,bp
        pop     bp
        ret
_display_char06 ENDP

;
; PixelAddr06
;       Determine buffer address of pixel in Mode 0x0E
;
; Inputs:
;       AX = y coordinate (0-199)
;       BX = x coordinate (0-640)
;
; Returns:
;       AH = bit mask
;       BX = byte offset in buffer
;       CL = number of bits to shift left
;       ES = video buffer segment
;
CGAVideoBufferSeg EQU	0B800h

PixelAddr06 PROC NEAR
	mov	cl,bl		; CL = low-order byte of x
	xchg	ah,al		; AX = 100h*y
	shr	bx,1		; x/2
	shr	ax,1		; 80h*(y&1)
	add	bh,al		; x/2 + 8000h*(y&1)
	xor	al,al		; 100h*(y/2)
	add	bx,ax		; x/2+8000h*(y&1)+140h*(y/2)
	shr	ax,1
	shr	ax,1		; 40h*(y/2)
	add	bx,ax		; x/2 + 8000h*(y&1) +140h*(y/2)
	shr	bx,1
	shr	bx,1		; x/8 + 2000h*(y&1) + 50h*(y/2)
	add	bx,OriginOffset ; BX == byte offset in video buffer
	mov	ax,CGAVideoBufferSeg
	mov	es,ax		; ES:BX = byte address of pixel
	and	cl,7
	xor	cl,7		; CL = nubmer of bits to shift left
	mov	ah,1		; unshifted bit mask
        ret
PixelAddr06 ENDP
	END


