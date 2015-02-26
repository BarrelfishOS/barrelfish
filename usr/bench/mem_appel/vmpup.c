/* vmpup.c
  
          VM-PUP Benchmark Program

	   Andrew W. Appel and Kai Li
	   Princeton University
	   Copyright (c) 1990, 1997

Introduction:

   The VM-PUP "Virtual Memory Primitives for User Programs"
   is a measure of hardware and operating-system performance
   in allowing clients of the operating system to service
   memory-protection faults.  One VM-PUP provides one fault-service
   per millisecond.  The VM-PUP is measured by this
   benchmark program (as suitably modified to suit the host system's
   interface for user-mode virtual-memory calls) as described
   under "Definition of a VM-PUP" below.   

Citations and sources:

    This program:
      http://www.cs.princeton.edu/~appel/papers/vmpup.c

    The "VM-PUP" benchmark program was used to get measurements
    described in the paper,

       Virtual memory primitives for user programs. 
       Andrew W. Appel and Kai Li. 
       Proc. Fourth International Conference on Architectural Support for 
          Programming Languages and Operating Systems (ASPLOS-IV),
      (ACM SIGPLAN Notices 26(4)) pp. 96-107, April 1991.
      http://www.cs.princeton.edu/~appel/papers/vmpup.ps

Why would you want to measure this?
   
   Abstract of the ASPLOS paper cited above:

   Memory Management Units (MMUs) are traditionally used by operating
   systems to implement disk-paged virtual memory.  Some operating systems
   allow user programs to specify the protection level (inaccessible,
   read-only, read-write) of pages, and allow user programs to handle
   protection violations, but these mechanisms are not always robust,
   efficient, or well-matched to the needs of applications.
   
   We survey several user-level algorithms that make use of page-protection
   techniques, and analyze their common characteristics, in an attempt
   to answer the question, ``What virtual-memory primitives should the
   operating system provide to user processes, and how well do today's
   operating systems provide them?''
   
What the benchmark does:

    User programs that use virtual memory will want to 
    0. Perform a register-register add.  This gives some idea of
         the machine's peak instruction-issue rate.
    1. trap on a protected page, handle the trap in user mode,
         inside the trap handler, protect some other page,
         unprotect the trapping page, and then return from the
         trap handler.  This is called "prot1+trap+unprot".
         Times are measured in microseconds per trap.

    2. Protect 100 contiguous pages, then as each page traps, handle the
          trap in user mode, unprotect the page, and resume
          from the trap handler.  This is called "protN+trap+unprot";
          times are measured in microseconds per trap (but include
          the time to protect 100 pages, amortized over 100 traps).

    3.  Trap on a protected page, enter a user-mode handler,
          return from the user mode handler.  This is called "trap";
	  time is measured in microseconds per trap.

How to invoke:

   Here's an example of running the benchmark on a Sun3/60 (a machine
   that was already obsolete in 1990):

   % a.out sun3/60 SunOS4.0     
   sun3/60 & SunOS4.0 & % machine and operating system
   0.120 &  0.000 &  0.120 & %     0.1 add
    142 &   1094 &   1238 & %    12.4 prot1+trap+unprot
   88.00 & 924.00 & 1016.00 & %    10.2 protN+trap+unprot: 100 pages
   64.00 & 692.00 & 760.00 & %     3.8 trap only
   1.00 & % VM-PUP rating

   First column is user-time, second column is system-time, third column
   is elapsed time.  Elapsed time is reported in the ASPLOS paper
   for several 1990-era machines are reported in the paper.

Definition of a VM-PUP:

   The Sun3/60 can d 1.00 VM-PUP's, taking the following formula:

   3000 microseconds / (     time for prot1+trap+unprot 
                        plus time for protN+trap+unprot
                        plus time for trap only) 

   that is,  3000 / (1238 + 1016 + 760) = 1.00

   Another way of saying this is that k VM-PUPs means the machine
   can do k Virtual-Memory-Primitives-for-User-Program per millisecond.

Caveats:

   For modern machines, you may need to adjust the loop
   to run for more interations, and also adjust the output format
   to allow for numbers that are a factor-of-100 smaller.

History:

  July 13, 1990.  Original version of benchmark program.

  November 27, 1990.  Andrew W.  Appel and Kai Li.
                       This revision of the benchmark program:
     1. In the trap handler, first a page is protected, then the trapping
           page is unprotected (this fixes a bug pointed out by
	   Rick Rashid).
     2. We measure the time to do a trap without any protection or 
           unprotection.
     3. The time for an add is printed out with more precision.
     4. The number of pages is now only 100, with 100 rounds instead of 10.
       
  March 10, 1997.    Andrew W. Appel.  
      Added the introductory comment.
      Added the call to do_nothing to make it work on the Dec Alpha.
      Added the last printf statement to print the VM-PUP rating.  

*/


#include <stdio.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/resource.h>

#define ALL_PRIVILEGES	  (PROT_READ|PROT_WRITE|PROT_EXEC)

#define PAGES 100
#define ADDS 1000000
#define ROUNDS 100
#define TRAPS 5000

int pagesize;
int bogo1,bogo2;
char *mem, *please_protect, *please_unprotect;
struct rusage t0, t1, t2, t3, t4,t5,t6,t7,t8;
struct timeval tod0, tod1, tod2, tod3,tod4, tod5, tod6, tod7, tod8;
int perm[PAGES];

int usrtime(a,b) struct rusage a, b;
{return (b.ru_utime.tv_sec-a.ru_utime.tv_sec)*1000000
   + (b.ru_utime.tv_usec-a.ru_utime.tv_usec);
}

int systime(a,b) struct rusage a, b;
{return (b.ru_stime.tv_sec-a.ru_stime.tv_sec)*1000000
   + (b.ru_stime.tv_usec-a.ru_stime.tv_usec);
}
 
int tod(a,b) struct timeval a,b;
{return (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_usec - a.tv_usec);
}

rand()
{static  long    randx = 1;
 return((randx = randx * 1103515245 + 12345) & 0x7fffffff);
}

int count=0;

void nop() {}
void (*do_nothing)() =  nop;

void handler()
{
 count++;
 if (count >= 0)
   {if (please_protect) mprotect(please_protect, pagesize, 0);
    mprotect(please_unprotect, pagesize, ALL_PRIVILEGES);
   }
}

main(argc,argv) int argc; char **argv;
{register int i, j, k;
 if (argc<3) {printf("Please run:   %s machine-type opsys\n\
For example:  %s Dec3100 Ultrix3.1\n",argv[0], argv[0]); 
	      exit(1);}
 printf("%s & %s & %% machine and operating system\n",argv[1],argv[2]);
 signal(SIGSEGV,handler);
 pagesize= getpagesize();
 mem = (char *)(((long) malloc((PAGES+1) * pagesize) + pagesize - 1)
		& ~(pagesize-1));
 for (i=0;i<PAGES;i++)
   mem[i*pagesize]=20;
 for(i=1;i<PAGES;i++) perm[i]=i;
 for(i=1;i<PAGES-1;i++)
   {j=rand()%(PAGES-i-1);
    k=perm[j+i]; perm[j+i]=perm[i]; perm[i]=k;
   }
 getrusage(0,&t1); gettimeofday(&tod1,0);
 i=ADDS/20; j=0;
 do {j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;
     j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;j+=k;}
 while (--i>0);
 getrusage(0,&t2); gettimeofday(&tod2,0);
 for(i=0;i<ROUNDS;i++)
   {mprotect(mem,PAGES*pagesize,0);
    for(j=0;j<PAGES;j++)
      {please_unprotect= mem+perm[j]*pagesize;
       please_protect= 0;
       *please_unprotect=10;
      }
   }
 getrusage(0,&t3); gettimeofday(&tod3,0);
 mprotect(mem,PAGES*pagesize,0);
 mprotect(mem+perm[PAGES-1]*pagesize, pagesize, ALL_PRIVILEGES);
 please_protect= mem+perm[PAGES-1]*pagesize;
 *please_protect=0;
 getrusage(0,&t4); gettimeofday(&tod4,0);
 for(i=0;i<ROUNDS;i++)
   for(j=0;j<PAGES;j++)
      {please_unprotect= mem+perm[j]*pagesize;
       do_nothing();  /* prevents over-optimization here! */
       *please_unprotect=10;
       please_protect=please_unprotect;
      }
 getrusage(0,&t5); gettimeofday(&tod5,0);

 if (count != 2*ROUNDS*PAGES)
  printf("Operating system bug, only %d traps instead of %d\n",
	  count, 2*ROUNDS*PAGES);
 count = -TRAPS;
 please_unprotect=mem+perm[0]*pagesize;
 while (count<0) /* this is a loop only to satisfy the 68020, which
		    returns from the trap AFTER the trapping instruction! */
       *please_unprotect=10;
 getrusage(0,&t6); gettimeofday(&tod6,0);

 printf("%4.3f & %6.3f & %6.3f & %% %7.1f add\n",
	usrtime(t1,t2)/(double)ADDS, systime(t1,t2)/(double)ADDS,
	tod(tod1,tod2)/(double)ADDS,	tod(tod1,tod2)/(double)1e6);
 printf("%4.0f & %6.0f & %6.0f & %% %7.1f prot1+trap+unprot\n",
	usrtime(t4,t5)/(double)(ROUNDS*PAGES), systime(t4,t5)/(double)(ROUNDS*PAGES),
	tod(tod4,tod5)/(double)(ROUNDS*PAGES),	tod(tod4,tod5)/(double)1e6);
 printf("%4.2f & %6.2f & %6.2f & %% %7.1f protN+trap+unprot: %d pages\n",
	usrtime(t2,t3)/(double)(ROUNDS*PAGES), systime(t2,t3)/(double)(ROUNDS*PAGES),
	tod(tod2,tod3)/(double)(ROUNDS*PAGES),tod(tod2,tod3)/(double)1e6,PAGES);
 printf("%4.2f & %6.2f & %6.2f & %% %7.1f trap only\n",
	usrtime(t5,t6)/(double)TRAPS, systime(t5,t6)/(double)TRAPS,
	tod(tod5,tod6)/(double)TRAPS,tod(tod5,tod6)/(double)1e6);
 printf("8.2f & % VM-PUP rating\n", 3000.0 / 
              ((tod(tod4,tod5)+tod(tod2,tod3)+tod(tod5,tod6))/
               (double)(ROUNDS*PAGES)));
 exit(0);
}
