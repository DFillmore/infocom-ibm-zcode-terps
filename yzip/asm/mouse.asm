	TITLE	rodentia

_TEXT	SEGMENT BYTE PUBLIC 'CODE'
_TEXT	ENDS
NULL	SEGMENT WORD PUBLIC 'BEGDATA'
NULL	ENDS
_DATA	SEGMENT WORD PUBLIC 'DATA'
EXTRN	_mouse_event_count:WORD
EXTRN	_mouse_queue_start:WORD
EXTRN	_mouse_event:WORD
EXTRN	_mouse_x:WORD
EXTRN	_mouse_y:WORD
_DATA	ENDS
CONST	SEGMENT WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT WORD PUBLIC 'BSS'
_BSS	ENDS

DGROUP	GROUP CONST, _BSS, _DATA, NULL
	ASSUME CS: _TEXT, DS:DGROUP, SS:DGROUP, ES:DGROUP

_TEXT	SEGMENT
PUBLIC	_mouse, _get_mouse_event
; This is the interrupt handler.  Called with:
; AX = condition mask
; BX = button state
; CX = horizontal position (pixels)
; DX = vertical position (pixels)

_mouse	PROC	FAR 
	push	bp
	mov	bp,sp
	test	ax,6
	jz	mouret
	push	ds
	push	si
	push	di
	push	bx
	push	ax		; get a register
	mov	bx,_data
	mov	ds,bx		; get data space
	mov	ax,_mouse_event_count
	cmp	ax,4
	jge	moustr
	inc	_mouse_event_count
	jmp	short mouadd
moustr:	inc	_mouse_queue_start
mouadd:	mov	bx,_mouse_event_count
	dec	bx
	add	bx,_mouse_queue_start
	and	bx,3			; get the actual index
	shl	bx,1
	mov	di,bx
	mov	_mouse_x[di],cx
	mov	_mouse_y[di],dx
	pop	bx
	pop	ax
	mov	_mouse_event[di],ax
	pop	di
	pop	si
	pop	ds
mouret:	mov	sp,bp
	pop	bp
	ret			; ???

_mouse	ENDP

; Get an event from the queue, return 0 or 1 (0 means no event pending).
; BEWARE--RUNS WITH INTERRUPTS DISABLED
; first argument is three word table that gets data smashed into it.

_get_mouse_event PROC NEAR
	push	bp
	mov	bp,sp
	push	si
	push	di
	cli			; no interrupts
	mov	ax,_mouse_event_count
	cmp	ax,0
	jz	gmeout
	mov	ax,_mouse_queue_start
	dec	_mouse_event_count
	jle	mqsclr
	inc	_mouse_queue_start
	jmp	short gmecnt
mqsclr:	xor	bx,bx
	mov	_mouse_queue_start,bx
gmecnt: and	ax,3
	shl	ax,1
	mov	si,ax
	mov	di,[bp+4]
	mov	ax,_mouse_event[si]
	mov	[di],ax
	mov	ax,_mouse_x[si]
	mov	2[di],ax
	mov	ax,_mouse_y[si]
	mov	4[di],ax
	mov	ax,1
gmeout:	sti
	pop	di
	pop	si
	mov	sp,bp
	pop	bp
	ret

_get_mouse_event	ENDP

_TEXT	ENDS
END
