.asm.obj:
	masm $*;

.c.obj:
	msc /DXZIP /Fselst /Zi /Od $*.c;

ezip.obj: ezip.c zipdefs.h xzipvers.h

loop.obj: loop.c zipdefs.h

status.obj: status.c zipdefs.h

read.obj: read.c zipdefs.h

error.obj: error.c zipdefs.h

sysdep.obj: sysdep.c zipdefs.h

virtual.obj: virtual.c zipdefs.h

game.obj: game.c zipdefs.h

data.obj: data.c zipdefs.h xzipvers.h

disk.obj: disk.obj zipdefs.h

farread.obj: farread.asm

mouse.obj: mouse.asm

lib.obj: lib.asm

speed.obj: speed.asm

xzip.exe: ezip.obj loop.obj status.obj read.obj error.obj \
sysdep.obj virtual.obj game.obj data.obj disk.obj farread.obj \
mouse.obj lib.obj speed.obj
	link /STACK:1024 /CP:1 /CO ezip loop status read \
error sysdep \
virtual game data disk farread mouse lib speed,xzip,xzip,;
