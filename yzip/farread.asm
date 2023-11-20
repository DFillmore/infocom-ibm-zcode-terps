	TITLE	far_read

_TEXT	SEGMENT	BYTE PUBLIC 'CODE'
_TEXT	ENDS
NULL	SEGMENT WORD PUBLIC 'BEGDATA'
NULL	ENDS
_DATA	SEGMENT WORD PUBLIC 'DATA'
EXTRN	_printabt:BYTE
EXTRN	_diskabt:BYTE
EXTRN	_cridevi:WORD
tret	dw	0
_DATA	ENDS
_BSS	SEGMENT	WORD PUBLIC 'BSS'
_BSS	ENDS

DGROUP	GROUP	_BSS, _DATA, NULL
	ASSUME	CS: _TEXT, DS: DGROUP, SS:DGROUP, ES:DGROUP

_TEXT	SEGMENT
EXTRN	_do_error:NEAR
; Args are file handle, segment, offset, # bytes 
	PUBLIC	_far_read,_far_write,_zopen,_zcreat,_crterr,_do_copy
	PUBLIC	_get_segreg,_zdumpfont,_zusrfont,_zclose

; return a segment register.  1 is cs, 2 is ds, 3 is es.

_get_segreg	PROC NEAR
	push	bp
	mov	bp,sp
	mov	ax,[bp+4]
	cmp	ax,1
	je	gsr1
	cmp	ax,2
	je	gsr2
	mov	ax,es
	jmp	short gsrdon
gsr1:	mov	ax,cs
	jmp	short gsrdon
gsr2:	mov	ax,ds
gsrdon:	mov	sp,bp
	pop	bp
	ret
_get_segreg	ENDP



; Args are: seg of source (or 0 for current ds), offset of source,
; seg of dest (or 0), offset of dest, number of BYTES to copy.
_do_copy	PROC NEAR
	push	bp
	mov	bp,sp
	push	si
	push	di
	push	es
	push	ds
	push	cx
	mov	cx,[bp+8]
	cmp	cx,0
	jne	dc1
	mov	cx,ds
dc1:	mov	es,cx
	mov	cx,[bp+4]
	cmp	cx,0
	jne	dc2
	mov	cx,ds
dc2:	mov	ds,cx
	mov	si,[bp+6]
	mov	di,[bp+10]
	mov	cx,[bp+12]
	cld
	rep	movsb			; Do the copy
	pop	cx
	pop	ds
	pop	es
	pop	di
	pop	si
	mov	sp,bp
	pop	bp
	ret

_do_copy	ENDP

_far_write	PROC NEAR

	push	bp
	mov	bp,sp
	mov	_diskabt,0
	push	ds
	mov	ah,40H		; Write Handle
	mov	bx,[bp+6]
	cmp	bx,0
	je	fwnos
	mov	ds,bx
fwnos:	mov	bx,[bp+4]
	mov	cx,[bp+10]
	mov	dx,[bp+8]
	int	21H
	jnc	fwret
	not	ax
	add	ax,1		; add one for correct negative number
fwret:	pop	ds
	mov	sp,bp
	pop	bp
	ret
_far_write	ENDP

_far_read	PROC NEAR

	push	bp
	mov	bp,sp		; our frame pointer?
	push	ds		; save the data segment
	mov	_diskabt,0
	mov	ah,3fH		; Read Handle function
	mov	bx,[bp+6]
	cmp	bx,0
	je	frnos
	mov	ds,bx
frnos:	mov	bx,[bp+4]	; handle
	mov	cx,[bp+10]	; last arg--# bytes
	mov	dx,[bp+8]	; offset
	int	21H		; do it
	jnc	frret
	not	ax		; return complement
	add	ax,1		; add one for correct negative number
frret:	pop	ds
	mov	sp,bp
	pop	bp
	ret

_far_read	ENDP

_zopen	PROC NEAR
	push	bp
	mov	bp,sp
	push	ds
	mov	ax,[bp+6]	; mode
	mov	ah,3dH		; Open Handle
	mov	dx,[bp+4]	; path
	int	21H		; do it
	jnc	zoret		; jump if won
	not	ax		; return will be negative...
	add	ax,1		; add one for correct negative number
zoret:	pop	ds
	mov	sp,bp
	pop	bp
	ret

_zopen	ENDP

_zusrfont PROC NEAR
	push	bp
	mov	bp,sp
	push	bp
	push	es
	push	ds
	pop	es
	mov	bp,[bp+4]
	mov	ah,11H
	mov	al,20H
	int	10H
	pop	es
	pop	bp
	mov	sp,bp
	pop	bp
	ret
_zusrfont	ENDP

_zdumpfont PROC	NEAR
	push	bp
	mov	bp,sp
	push	bp
	push	es
	push	ds
	pop	es
	mov	bp,[bp+4]
	mov	ah,11H
	mov	al,21H
        mov	cx,8H
	mov	bl,2
	int	10H
	pop	es
	pop	bp
	mov	sp,bp
	pop	bp
	ret

_zdumpfont	ENDP

_zclose PROC	NEAR
	push	bp
	mov	bp,sp
	mov	ah,3eH
	mov	bx,[bp+4]
	int	21H
	jc	zclret
	mov	ax,0
zclret:	mov	sp,bp
	pop	bp
	ret

_zclose	ENDP

_zcreat	PROC	NEAR

	push	bp
	mov	bp,sp
	push	ds
	mov	ah,3cH
	mov	cx,0		; unrestricted read/write
	mov	dx,[bp+4]
	int	21H
	jnc	zcret
	not	ax
	add	ax,1		; add one for correct negative number
zcret:	pop	ds
	mov	sp,bp
	pop	bp
	ret

_zcreat	ENDP

_crterr	PROC	NEAR
	sti		; re-enable interrupts
	push	bp	; segment address of device header?
	mov	bp,sp	; new base
	push	di
	mov	bx,_data
	mov	es,bx	; we're copying here...
	mov	ds,[bp]	; we'll hope this works
	mov	di,offset _cridevi
	mov	cx,9
	rep	movsw	; copy stuff into cridevi
	mov	bx,_data
	mov	ds,bx	; real data segment, for calling C code
	pop	di	; first arg (error code)
	push	di
	push	ax	; second arg (disk error code)
	mov	ax,offset _cridevi	; device header
	push	ax
	call	_do_error
	add	sp,6		; flush args
ceret:	xor	ah,ah		; clear ah, for fun
	mov	tret,ax		; for later
	mov	sp,bp
	pop	bp
	add	sp,6		; flush return to DOS
	pop	ax		; restore registers
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	bp
	pop	ds
	pop	es
	push	bp
	mov	bp,sp
	push	ax
	push	bx
	mov	ax,tret		; abort or retry?
	mov	bx,[bp+2]	; return address in user code
	cmp	ax,0
	je	cert1
	sub	bx,2		; this is retry, so back up return
	mov	[bp+2],bx
	and	WORD PTR [bp+6],0fffeH
;
; make sure carry not set
;
	jmp	short cert2
cert1:	or	WORD PTR [bp+6],1	; make sure carry set
cert2:	and	WORD PTR [bp+6],0fbffH	; clear direction flag
	pop	bx
	pop	ax
	mov	sp,bp
	pop	bp
	iret

_crterr	ENDP

_TEXT	ENDS
END

