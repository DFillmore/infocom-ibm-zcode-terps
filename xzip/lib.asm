	TITLE	library

_TEXT	SEGMENT	BYTE PUBLIC 'CODE'
_TEXT	ENDS
NULL	SEGMENT WORD PUBLIC 'BEGDATA'
NULL	ENDS
_DATA	SEGMENT WORD PUBLIC 'DATA'
	PUBLIC _environ,___argv,___argc,__psp,__osfile,__osmajor, __osminor
	PUBLIC __dosvermajor, __dosverminor, __osversion
	PUBLIC __doserrno, _errno
	PUBLIC _acfinfo, __child, __fac, __intno, __nfile, __oserr
	PUBLIC __osmode, __ovlflag, __ovlvec, __pgmptr
	PUBLIC __psprdr, __umaskval

_acfinfo dw 0
__child	     dw 0
__fac	     dw 0
__intno	     dw 0
__nfile	     dw 0
__oserr	     dw 0
__osfile     dw 0
__osmode     dw 0
__ovlflag    dw 0
__ovlvec     dw 0
__pgmptr     dw 0
__psprdr     dw 0
__umaskval	dw 0
__doserrno dw 0
_errno dw 0
__dosvermajor dw 0
__dosverminor dw 0
__osversion dw 0
_environ dw 0
___argv dw 0
___argc dw 0
__psp  dw 0

__osminor dw 0
__osmajor dw 0
_DATA	ENDS
CONST	SEGMENT	WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT	WORD PUBLIC 'BSS'
_BSS	ENDS

DGROUP	GROUP	CONST, _BSS, _DATA, NULL
	ASSUME	CS: _TEXT, DS: DGROUP, SS:DGROUP, ES:DGROUP

_TEXT	SEGMENT
	PUBLIC	_int86,_int86x,_kbhit
	PUBLIC __dosret0, __dosretax, __cinit, _exit, __exit, __ctermsub
	PUBLIC __NMSG_TEXT,__NMSG_WRITE
	PUBLIC __fptrap

__fptrap	PROC	NEAR
	ret
__fptrap	ENDP

__NMSG_TEXT	PROC	NEAR
	ret
__NMSG_TEXT	ENDP

__NMSG_WRITE	PROC	NEAR
	ret
__NMSG_WRITE	ENDP

__ctermsub	PROC	NEAR
	ret
__ctermsub	ENDP

_exit	PROC	NEAR
	xor	ax,ax
	call	__exit
_exit	ENDP

__exit	PROC	NEAR
	xor	al,al
	mov	ah,4CH
	int	21H
__exit	ENDP

__cinit	PROC	NEAR
	ret
__cinit	ENDP

__dosret0	PROC	NEAR
	xor	ax,ax
	mov	sp,bp
	pop	bp
	ret
__dosret0	ENDP

__dosretax	PROC	NEAR
	mov	sp,bp
	pop	bp
	ret
__dosretax	ENDP

_int86	PROC	NEAR
	push	bp		; transcribed from library
	mov	bp,sp
	push	si
	push	di
	sub	sp,0AH		; make space on the stack
	mov	BYTE PTR [bp-0AH],0CDH
	mov	ax,[bp+4]
	mov	[bp-9],al
	mov	BYTE PTR [bp-8],0CBH
	mov	[bp-0CH],ss
	lea	ax,[bp-0AH]
	mov	[bp-0EH],ax
	mov	di,[bp+6]
	mov	ax,[di]
	mov	bx,[di+2]
	mov	cx,[di+4]
	mov	dx,[di+6]
	mov	si,[di+8]
	mov	di,[di+0AH]
	push	bp
	sub	bp,0EH
	call	DWORD PTR [bp+0]
	pop	bp
	push	di
	mov	di,[bp+8]
	mov	[di],ax
	mov	[di+2],bx
	mov	[di+4],cx
	mov	[di+6],dx
	mov	[di+8],si
	pop	WORD PTR [di+0AH]
	jb	intlos
	xor	si,si
	jmp	intret
intlos:	mov	si,1
intret:	mov	ax,[di]
	mov	[di+0CH],si
	add	sp,0AH
	cld
	pop	di
	pop	si
	mov	sp,bp
	pop	bp
	ret
_int86	ENDP

_int86x	PROC	NEAR
	push	bp		; transcribed from library
	mov	bp,sp
	push	si
	push	di
	sub	sp,0AH		; make space on the stack
	mov	BYTE PTR [bp-0AH],0CDH
	mov	ax,[bp+4]
	mov	[bp-9],al
	mov	BYTE PTR [bp-8],0CBH
	mov	[bp-0CH],ss
	lea	ax,[bp-0AH]
	mov	[bp-0EH],ax
	mov	di,[bp+6]
	mov	ax,[di]
	mov	bx,[di+2]
	mov	cx,[di+4]
	mov	dx,[di+6]
	mov	si,[di+8]
	mov	di,[di+0AH]
	push	ds
	push	es
	push	ax
	mov	ax,[bp+0AH]		; which register to load
	cmp	ax,2			; DS?
	je	intds
	mov	es,[bp+0CH]		; es
	jmp	intxdo
intds:	mov	ds,[bp+0CH]
intxdo:	pop	ax
	push	bp
	sub	bp,0EH
	call	DWORD PTR [bp]		; this actually does the call
	pop	bp
	mov	ax,[bp+0AH]
	cmp	ax,2
	je	savds
	mov	[bp+0CH],es
	jmp	inrxrt
savds:	mov	[bp+0CH],ds
inrxrt:	pop	es
	pop	ds			; restore segment registers
	push	di
	mov	di,[bp+8]
	mov	[di],ax
	mov	[di+2],bx
	mov	[di+4],cx
	mov	[di+6],dx
	mov	[di+8],si
	pop	WORD PTR [di+0AH]
	jb	inxlos
	xor	si,si
	jmp	inxret
inxlos:	mov	si,1
inxret:	mov	ax,[di]
	mov	[di+0CH],si
	add	sp,0AH
	cld
	mov	ax,[bp+0CH]		; return the register we set...
	pop	di
	pop	si
	mov	sp,bp
	pop	bp
	ret
_int86x	ENDP

_kbhit	PROC	NEAR
	push	bp
	mov	bp,sp
	mov	ah,1
	int	16H		; keyboard service
	jz	nohit
	mov	ax,1
kbret:	mov	sp,bp
	pop	bp
	ret
nohit:	xor	ax,ax
	jmp	kbret
_kbhit	ENDP



_TEXT	ENDS
END

