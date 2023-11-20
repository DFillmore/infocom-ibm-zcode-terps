OBJDIR= .\obj\\

ASMDIR= ..\asm\\

ASMOPT= /mx -i\bin\tc\examples /W2 /Z /l /JMASM51 /JQUIRKS

!if !$d(RELDIR)
RELDIR= f:\ibmzip\yzip
!endif

!if !$d(CDEF)
CDEF=XXX
!endif

!if !$d(G)
G = SHOGUN
!elif !$d(NEW)
NEWGAME = newgame
!endif

CC= -tcc -D$(CDEF) -D$(G)

!if $d(RELEASE)
CFLAGS= -c -G -O -f- -k- -N- -y- -v- -d -DRELEASE
LFLAGS= 
SAVE_EXE=copy yzip.exe f:\ibmzip\yzip\$(G).exe
!else
CFLAGS= -c
LFLAGS= /v
SAVE_EXE=@echo All DONE!
!endif 

!if $d(COPY)
DO_REL=make_rel
!else
DO_REL=
!endif

!if $d(NEW)
CLEANUP=cleanup
!else
CLEANUP=
!endif


.asm.obj:
	tasm $(ASMIDIR)$&;

.c.obj:
	tcc $&.c

all: $(CLEANUP) $(DO_REL) $(NEWGAME) yzip.exe 

cleanup:
        del $(OBJDIR)*.obj
	del *.lst
	del *.err

newgame:
	del $(OBJDIR)pic.obj
	del $(OBJDIR)sysdep2.obj
	del $(OBJDIR)data.obj

make_rel:
	yzipbak $(RELDIR)

yzip.exe: $(OBJDIR)pic.obj \
	$(OBJDIR)yzip.obj \
	$(OBJDIR)loop.obj \
	$(OBJDIR)output.obj \
	$(OBJDIR)read.obj \
        $(OBJDIR)yzipvers.obj \
	$(OBJDIR)error.obj \
	$(OBJDIR)sysdep1.obj \
	$(OBJDIR)sysdep2.obj \
	$(OBJDIR)virtual.obj \
	$(OBJDIR)game.obj \
	$(OBJDIR)data.obj \
	$(OBJDIR)disk.obj \
	$(OBJDIR)farread.obj \
	$(OBJDIR)mouse.obj \
	$(OBJDIR)lib.obj\
	$(OBJDIR)speed.obj\
        $(OBJDIR)c0.obj \
	$(OBJDIR)fonts.obj \
        $(OBJDIR)windows.obj \
        $(OBJDIR)graphics.obj \
        $(OBJDIR)jd.obj \
	$(OBJDIR)scroll.obj \
        makefile \
        yzip.lnk
	-tlink $(LFLAGS) @yzip.lnk
	$(SAVE_EXE)

$(OBJDIR)pic.obj: pic.c zipdefs.h pic.h sysdep.h
	$(CC) $(CFLAGS) pic.c

$(OBJDIR)yzip.obj: yzip.c zipdefs.h pic.h sysdep.h
	$(CC) $(CFLAGS) yzip.c

$(OBJDIR)yzipvers.obj: yzipvers.c zipdefs.h
	$(CC) $(CFLAGS) yzipvers.c

$(OBJDIR)loop.obj: loop.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) loop.c

$(OBJDIR)output.obj: output.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) output.c

$(OBJDIR)read.obj: read.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) read.c

$(OBJDIR)fonts.obj: fonts.c zipdefs.h
	$(CC) $(CFLAGS) fonts.c

$(OBJDIR)error.obj: error.c zipdefs.h
	$(CC) $(CFLAGS) error.c

$(OBJDIR)sysdep1.obj: sysdep1.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) sysdep1.c

$(OBJDIR)windows.obj: windows.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) windows.c

$(OBJDIR)sysdep2.obj: sysdep2.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) sysdep2.c

$(OBJDIR)virtual.obj: virtual.c zipdefs.h
	$(CC) $(CFLAGS) virtual.c

$(OBJDIR)game.obj: game.c zipdefs.h
	$(CC) $(CFLAGS) game.c

$(OBJDIR)data.obj: data.c zipdefs.h sysdep.h
	$(CC) $(CFLAGS) data.c

$(OBJDIR)disk.obj: disk.c zipdefs.h
	$(CC) $(CFLAGS) disk.c

$(OBJDIR)farread.obj: $(ASMDIR)farread.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)farread,$(OBJDIR)farread,nul,nul

$(OBJDIR)mouse.obj: $(ASMDIR)mouse.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)mouse,$(OBJDIR)mouse,nul,nul

$(OBJDIR)lib.obj: $(ASMDIR)lib.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)lib,$(OBJDIR)lib,nul,nul

$(OBJDIR)c0.obj: $(ASMDIR)c0.asm
        -TASM $(ASMDIR)C0,$(OBJDIR)C0     /D__SMALL__   /MX $(ASMOPT),nul,nul

$(OBJDIR)speed.obj: $(ASMDIR)speed.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)speed,$(OBJDIR)speed,nul,nul

$(OBJDIR)ega.obj: $(ASMDIR)ega.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)ega,$(OBJDIR)ega,nul,nul

$(OBJDIR)graphics.obj: $(ASMDIR)graphics.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)graphics,$(OBJDIR)graphics,nul,nul

$(OBJDIR)scroll.obj: $(ASMDIR)scroll.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)scroll,$(OBJDIR)scroll,nul,nul

$(OBJDIR)jd.obj: $(ASMDIR)jd.asm
	-tasm $(ASMOPT) /zi $(ASMDIR)jd,$(OBJDIR)jd,nul,nul



