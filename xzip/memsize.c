#include <dos.h>
#include <stdio.h>

main(argc,argv)
int argc, argv;
{
  union REGS regs;
  int oldmode, newmode;
  regs.h.ah = 0xf;
  int86(0x10, &regs, &regs);
  oldmode = regs.h.al;
  regs.h.ah = 0;
  regs.h.al = 0xa;
  int86(0x10, &regs, &regs);
  regs.h.ah = 0xf;
  int86(0x10, &regs, &regs);
  newmode = regs.h.al;
  regs.h.ah = 0;
  regs.h.al = oldmode;
  int86(0x10, &regs, &regs);
  printf("Set to PCjr mode gives mode %i.\n",newmode);
}
