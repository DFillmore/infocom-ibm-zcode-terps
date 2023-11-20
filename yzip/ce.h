{bt
/* ce.h - critical error and asker function types */

#define CE_INTERRUPT     0x24

void interrupt CE_trap(void);

typedef int (far *askerfcnptr)(int, int, int, int);
void far setCEasker(askerfcnptr);
askerfcnptr far getCEasker(void);

typedef union {
  unsigned short ax;
  struct {
    unsigned drive : 8;      /* 0=A, 1=B, ... */
    unsigned r_w : 1;        /* 0=read; 1=write */
    unsigned area  : 2;      /* 0=DOS sys; 1=FAT; 2=dir; 3=data */
    unsigned actions : 3;
    unsigned       : 1;
    unsigned typ   : 1;      /* 0=disk error; 1=non-disk */
  } parts;
} AXBITS;

typedef union {
  unsigned short di;
  struct {
    unsigned char errorcode;
    unsigned char di_hi;
  } parts;
} DIBITS;
{et
