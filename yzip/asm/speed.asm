	TITLE	speed

_TEXT	SEGMENT	BYTE PUBLIC 'CODE'
_TEXT	ENDS
NULL	SEGMENT WORD PUBLIC 'BEGDATA'
NULL	ENDS
_DATA	SEGMENT WORD PUBLIC 'DATA'
EXTRN	_dataspace:DWORD
EXTRN	_curblkloc:DWORD
EXTRN	_zpc1:WORD
EXTRN	_zpc2:WORD
EXTRN	_scrx:BYTE
EXTRN	_scry:BYTE
EXTRN	_screen:BYTE
_DATA	ENDS
CONST	SEGMENT	WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT	WORD PUBLIC 'BSS'
_BSS	ENDS

DGROUP	GROUP	CONST, _BSS, _DATA, NULL
	ASSUME	CS: _TEXT, DS: DGROUP, SS:DGROUP, ES:DGROUP

_TEXT	SEGMENT
	PUBLIC	_GTAWRD, _GTVWRD, _PTVWRD, _nxtbyt
	PUBLIC	_do_table_print, _do_intbl, _do_table_copy
	EXTRN	_newzpc:NEAR, _locate:NEAR, _datbyt:NEAR, _putchr:NEAR
	EXTRN	_getpag:NEAR, _flush_buffer:NEAR, _ppred:NEAR, _putval:NEAR

; Return the word at the pointer provided.
_GTAWRD	PROC	NEAR
	push	bp
	mov	bp,sp
	les	bx,DWORD PTR [bp+4]		; pick up the pointer
	mov	ax,es:[bx]
	xchg	ah,al				; right order...
	mov	sp,bp
	pop	bp
	ret
_GTAWRD	ENDP

; Return the word at the offset provided.  This assumes the offset is less
; than 64k, meaning we're in table space.  We can then safely add the offset
; to the offset part of the dataspace pointer to win...

_GTVWRD	PROC	NEAR
	push	bp
	mov	bp,sp
	les	bx,_dataspace
	add	bx,[bp+4]
	mov	ax,es:[bx]
	xchg	ah,al
	mov	sp,bp
	pop	bp
	ret
_GTVWRD	ENDP

_PTVWRD	PROC	NEAR
	push	bp
	mov	bp,sp
	les	bx,_dataspace
	add	bx,[bp+4]
	mov	ax,[bp+6]
	xchg	ah,al
	mov	es:[bx],ax
	mov	sp,bp
	pop	bp
	ret
_PTVWRD	ENDP

_nxtbyt	PROC	NEAR
	push	bp
	mov	bp,sp
	les	bx,_curblkloc
	add	bx,_zpc2
	mov	al,es:[bx]
	xor	ah,ah
	inc	_zpc2
	cmp	_zpc2,512
	jl	nxb1
	push	ax
	call	_newzpc
	pop	ax
nxb1:	mov	sp,bp
	pop	bp
	ret
_nxtbyt	ENDP

_do_table_print	PROC	NEAR
	push	si
	push	bp
	mov	bp,sp
	cmp	WORD PTR [bp+10],1
	jle	dtp1
	cmp	_screen,0
	jne	dtp1
	jmp	dtpend
dtp1:
	cmp	WORD PTR [bp+8],0		; no width
	jne	dtp2
	jmp	dtpend
dtp2:	cmp	WORD PTR [bp+10],0		; no height
	jne	dtp3
	jmp	dtpend
dtp3:	mov	al,_scry
	push	ax
	mov	al,_scrx
	push	ax
	mov	si,[bp+8]
	mov	ax,[bp+6]		; table pointer
	mov	cl,9
	shr	ax,cl			; get block number in ax
	push	ax			; save it
	mov	ax,[bp+6]
	and	ax,1FFH			; offset in block
	push	ax			; save it
	push	WORD PTR [bp-6]
	call	_getpag			; get a page pointer
	add	sp,2			; flush the arg
	mov	bx,ax
	mov	es,dx			; set up pointers
	add	bx,[bp-8]			; point to the right byte
dtplop:	push	bx			; save the pointers
	push	es
	mov	al,es:[bx]
	xor	ah,ah
	push	ax
	call	_putchr			; dump a character
	add	sp,2
	pop	es			; restore the pointers
	pop	bx
	dec	si			; decrement width
	jz	nxtlin			; jump if done with line
	inc	bx			; move to next character
	test	bx,1ffH			; see if done with page
	jz	nxtpag			; end of page
	jmp	dtplop			; just keep chugging
nxtlin:	dec	WORD PTR [bp+10]	; here if done with line.  dec height
	jz	dtpend			; all done
	push	bx
	push	es
	call	_flush_buffer		; nope, write buffer
	push	WORD PTR [bp-4]
	mov	ax,WORD PTR [bp-2]
	add	ax,8
	mov	WORD PTR [bp-2],ax
	push	ax
	call	_locate			; move the cursor
	add	sp,4
	pop	es
	pop	bx
	mov	ax,[bp-8]		; saved block offset
	add	ax,[bp+8]		; update it
	add	ax,[bp+12]
	cmp	ax,512			; are we past the page?
	jge	nxtlpg
	mov	[bp-8],ax		; nope, save for next time
	inc	bx
	add	bx,[bp+12]		; update the other offset
	mov	si,[bp+8]		; new count field
	jmp	dtplop			; go
nxtlpg:	inc	WORD PTR [bp-6]		; next page
	sub	ax,512
	mov	[bp-8],ax		; correct offset
	push	WORD PTR [bp-6]
	call	_getpag			; get new page pointer
	add	sp,2
	mov	es,dx
	mov	bx,ax
	add	bx,[bp+12]
	mov	si,WORD PTR [bp+8]
	jmp	dtplop
nxtpag:	inc	WORD PTR [bp-6]
	push	WORD PTR [bp-6]
	call	_getpag
	add	sp,2
	mov	es,dx
	mov	bx,ax
	xor	ax,ax
	mov	[bp-8],ax		; page offset is 0
	jmp	dtplop
dtpend:	mov	sp,bp
	pop	bp
	pop	si
	ret
_do_table_print	ENDP

_do_intbl	PROC	NEAR
	push	si
	push	di
	push	bp
	mov	bp,sp
	mov	ax,[bp+14]		; recspec
	xor	bx,bx
	test	ax,80H
	jz	di1
	xor	bx,-1
di1:	push	bx			; -1 if comparing words
	and	ax,07FH
	push	ax			; record length
	mov	ax,[bp+10]		; table
	mov	cl,9
	shr	ax,cl			; page number
	push	ax
	mov	ax,[bp+10]
	and	ax,1FFH			; offset
	mov	di,ax			; keep it handy
	push	WORD PTR [bp-6]
	call	_getpag			; get a page
	add	sp,2
	mov	es,dx
	add	ax,di			; make the offset right
	mov	si,ax			;es:[si] points to table
dimlop:	inc	WORD PTR [bp-2]
	jz	dicmpw
	xor	ah,ah
dilpc:	mov	al,es:[si]
	cmp	ax,[bp+8]
	je	diwon
	dec	WORD PTR [bp+12]
	jle	dilost
	add	si,[bp-4]
	add	di,[bp-4]
	cmp	di,512			; see if on next page
	jge	dinxtp
	jmp	dilpc
dicmpw:	mov	ax,es:[si]
	xchg	ah,al
	cmp	ax,[bp+8]
	je	diwon
	dec	WORD PTR [bp+12]
	jle	dilost
	add	si,[bp-4]
	add	di,[bp-4]
	cmp	di,512
	jl	dicmpw
	mov	ax,-1
	mov	[bp-2],ax
dinxtp:	inc	WORD PTR [bp-6]
	push	WORD PTR [bp-6]
	call	_getpag
	add	sp,2
	sub	di,512
	add	ax,di			; fix up offset
	mov	si,ax
	mov	es,dx
	jmp	dimlop
diwon:	mov	ax,[bp-6]		; page number
	mov	cl,9
	shl	ax,cl			; make it real
	add	ax,di			; add to pointer
	push	ax			; and return it
	call	_putval
	add	sp,2
	mov	ax,1			; also do the jump
	push	ax
	call	_ppred
	add	sp,2
diexit:	mov	sp,bp
	pop	bp
	pop	di
	pop	si
	ret
dilost:	xor	ax,ax
	push	ax
	call	_putval
	add	sp,2
	xor	ax,ax
	push	ax
	call	_ppred
	add	sp,2
	jmp	diexit

_do_intbl	ENDP

_do_table_copy	PROC	NEAR
	push	si
	push	di
	push	bp
	mov	bp,sp
	mov	bx,[bp+12]		; length
	cmp	bx,0
	jnz	dtc1
	jmp	short dtcret			; nothing to do
dtc1:	mov	ax,[bp+10]		; dest
	cmp	ax,[bp+8]		; same as source?
	jne	dtc2			; nope, got to do it
	jmp	short dtcret
dtc2:	cmp	ax,0
	jne	dtccop
; Here to zero a table (this will be fun)
	cld
	mov	ax,[bp+8]
	mov	cl,9
	shr	ax,cl
	push	ax			; page number
	call	_getpag			; get a pointer
	add	sp,2
	mov	es,dx
	mov	di,ax
	mov	ax,[bp+8]
	and	ax,1ffH			; get offset in page
	add	di,ax			; es:[di] is destination
	mov	cx,[bp+12]		; bytes to zero
	cmp	cx,0
	jg	dtc3
	mov	cx,[bp+12]
	neg	cx			; bozo gave us neg length
dtc3:	xor	al,al			; with 0, we hope
rep	stosb				; zero the sucker
dtcret:	mov	sp,bp
	pop	bp
	pop	di
	pop	si
	ret
; here to actually to string-to-string copy.  bx is length (also on stack),
; ax is dest.
dtccop:	cmp	bx,0			; if negative,...
	jge	dtcc1
	jmp	dtcfwd			; force forward copy
dtcc1:	cmp	ax,[bp+8]		; if dest < source, do forward
	jl	dtcfwd
	sub	ax,bx			; dest - length
	cmp	ax,[bp+8]		; dest - length >= source?
	jge	dtcfwd			; if so, we can do forward
; oops, have to do backwards copy, but at least we know the length is
; positive
	mov	ax,[bp+8]		; pick up source
	add	ax,[bp+12]
	dec	ax			; point to last byte to be copied
	push	ax			; let's save this...
	mov	cl,9
	shr	ax,cl
	push	ax			; [bp-4] will be page #
	push	ax			; let's save this, too
	call	_getpag			; last source page
	add	sp,2
	push	dx			; save what will be ds contents
	mov	si,ax
	mov	ax,[bp-2]		; saved address of end
	and	ax,1ffH			; offset in last page
	add	si,ax
	push	ax			; we'll need this in a second
	mov	ax,[bp+10]		; pick up dest
	add	ax,[bp+12]
	dec	ax			; point to last target byte
	push	ax
	mov	cl,9
	shr	ax,cl
	push	ax
	call	_getpag			; get last target page
	add	sp,2
	mov	es,dx
	mov	di,ax
	pop	ax			; last target byte again
	and	ax,1ffH			; offset in page
	add	di,ax			; ready to go
	pop	cx			; offset in last source page
	inc	cx			; offset 1 at this point means 2 bytes
dbclop:	std
	cmp	cx,[bp+12]		; compare to length
	jle	dbc2
	mov	cx,[bp+12]		; don't copy more than you need
dbc2:	sub	[bp+12],cx		; update length
	pop	dx			; remember me?
	push	ds
	mov	ds,dx
rep	movsb				; do a batch
	cld				; reset direction flag
	pop	ds			; restore ds
	cmp	WORD PTR [bp+12],0
	jle	dtcret			; all done
	dec	WORD PTR [bp-4]		; go to previous page
	push	es			; es can get trashed
	push	WORD PTR [bp-4]
	call	_getpag			; get another source page
	add	sp,2
	pop	es
	push	dx			; this will pop presently
	mov	si,ax
	add	si,511			; point to last byte of page
	mov	cx,512			; max on this page
	jmp	dbclop			; go do it
; lets do a forward copy
dtcfwd:	cmp	bx,0
	jge	dfc1			; length is OK
	neg	bx
	mov	[bp+12],bx		; length is OK now
dfc1:	mov	ax,[bp+10]		; destination
	mov	cl,9
	shr	ax,cl
	push	ax
	call	_getpag			; destination page
	add	sp,2
	mov	es,dx
	mov	di,ax
	mov	ax,[bp+10]
	and	ax,1ffH
	add	di,ax			; es:[di] is ready
	mov	ax,[bp+8]		; source
	mov	cl,9
	shr	ax,cl
	push	ax			; save source page
	push	ax
	call	_getpag			; get source page
	add	sp,2
	push	ds			; save ds
	mov	ds,dx
	mov	si,ax
	mov	ax,[bp+8]
	and	ax,1ffH
	add	si,ax			; point to first source byte
	mov	cx,512			; bytes/page
	sub	cx,ax			; number we can copy here
dfclop:	cld				; make sure direction is OK
	cmp	cx,[bp+12]		; it's all on this page
	jl	dfc4			; if we don't jump
	mov	cx,[bp+12]
dfc4:	sub	[bp+12],cx		; update the length
rep	movsb				; copy a few
	pop	ds			; make sure ds is here
	cmp	WORD PTR [bp+12],0
	jg	dfc5
	jmp	dtcret			; all done
dfc5:	inc	WORD PTR [bp-2]			; next source page
	push	es
	push	WORD PTR [bp-2]
	call	_getpag
	add	sp,2
	pop	es
	push	ds
	mov	ds,dx
	mov	si,ax
	mov	cx,512
	jmp	dfclop

_do_table_copy	ENDP

_TEXT	ENDS
END

