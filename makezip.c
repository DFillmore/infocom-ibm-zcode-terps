#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include <stdio.h>

main(argc,argv)
int argc;
char **argv;
{
   char *zipname;
   char *datname;
   char *gamename;
   char *copyright;
   int zipfd;
   unsigned char zipbuf[1024];
   int bytecnt;
   int savbytcnt;
   long curptr = 0;
   int offs = 0;
   int remaining = 7;
   char cchar;
   if (argc <= 4) {
     printf("Not enough arguments given.\n");
     return; }
   zipname = argv[1];		/* name of zip file */
   if (!(zipfd = open(zipname, O_BINARY | O_RDWR))) {
     printf("Can't open ZIP.\n");
     return; }
   datname = argv[2];
   gamename = argv[3];
   copyright = argv[4];
   bytecnt = read(zipfd, zipbuf, 1024);
   savbytcnt = 512;
   offs = 0;
   while (1) {
       cchar = zipbuf[offs];
       if (cchar == 'x') {
	 if (remaining & 4) {
	   if (check_frob(zipfd,zipbuf,curptr,offs,4,datname))
	     remaining = remaining & 3; } }
        else {
	 if (cchar == 'a') {
	  if (remaining & 2) {
	    if (check_frob(zipfd,zipbuf,curptr,offs,2,gamename))
	      remaining = remaining & 5; } }
	  else
	   if (cchar == 'z') {
	     if (remaining & 1) {
	       if (check_frob(zipfd,zipbuf,curptr,offs,1,copyright))
	         remaining = remaining & 6; } } }
       if (remaining == 0) break;
       if (++offs > savbytcnt) {	/* end of page */
	 if ((bytecnt <= 512) || (savbytcnt < 512)) {
	   printf("Warning:  %i remaining.\n",remaining);
	   break; }
	 curptr = curptr + offs - 1;
	 bytecnt = bytecnt - 512;
	 offs = bytecnt;
	 while (offs--) {
	   zipbuf[offs] = zipbuf[offs+512]; }
	 lseek(zipfd, curptr + 512, SEEK_SET); 	/* seek to current page */
	 offs = 0;
	 bytecnt = bytecnt + read(zipfd, &zipbuf[512], 512);
	 savbytcnt = bytecnt - 512; } }
  close(zipfd);
}

int check_frob(fd,buf,ptr,offs,which,msg)
int fd, offs, which;
long ptr;
char *msg, *buf;
{
  char cchar;
  int msglen;
  char *whichdesc;
  if (which == 4) {
    cchar = 'x';
    whichdesc = "file name";
    msglen = 12; }
   else
    if (which == 2) {
      cchar = 'a';
      whichdesc = "game name";
      msglen = 46; }
     else {
      cchar = 'z';
      whichdesc = "copyright date";
      msglen = 4; }
  while (msglen--) {
   if (buf[offs+msglen] != cchar) return(0); }
  printf("Adding %s at %li.\n", whichdesc, ptr + offs);
  while (1) {
   if ((cchar = *msg++) == '_')
     cchar = ' ';
   buf[offs+(++msglen)] = cchar;
   if (cchar == 0) break; }
  lseek(fd,ptr,SEEK_SET);
  write(fd, buf, offs + msglen + 1);
  return(1); }
