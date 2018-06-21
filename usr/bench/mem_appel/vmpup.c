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
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <inttypes.h>


#if defined(BARRELFISH)
#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
//#include <barrelfish/threads.h>
//#include <barrelfish/sys_debug.h>
#endif

#define PAGES 100
#define ADDS 1000000
#define ROUNDS 1000
#define TRAPS 5000

int pagesize;
int bogo1, bogo2;
char *please_protect, *please_unprotect;
struct rusage t0, t1, t2, t3, t4, t5, t6, t7, t8;
struct timeval tod0, tod1, tod2, tod3, tod4, tod5, tod6, tod7, tod8;
uint64_t ticks0, ticks1, ticks2, ticks3, ticks4, ticks5, ticks6;

// a permutation of the pages
int perm[PAGES];

static int __attribute__((unused))
usrtime(struct rusage a, struct rusage b)
{
    return (b.ru_utime.tv_sec - a.ru_utime.tv_sec) * 1000000 +
           (b.ru_utime.tv_usec - a.ru_utime.tv_usec);
}

static int __attribute__((unused))
systime(struct rusage a, struct rusage b)
{
    return (b.ru_stime.tv_sec - a.ru_stime.tv_sec) * 1000000 +
           (b.ru_stime.tv_usec - a.ru_stime.tv_usec);
}

static int __attribute__((unused))
tod(struct timeval a, struct timeval b)
{ return (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_usec - a.tv_usec); }

static int xrand(void) {
    static long randx = 1;
    return ((randx = randx * 1103515245 + 12345) & 0x7fffffff);
}

int count = 0;

static void nop(void) {}
void (*do_nothing)(void) = nop;

#if defined(__linux__)
#define ALL_PRIVILEGES (PROT_READ | PROT_WRITE | PROT_EXEC)
#define NO_PRIVILEGES  PROT_NONE
const char osName[] = "Linux";
#elif defined(BARRELFISH)
#define ALL_PRIVILEGES (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_EXECUTE)
#define NO_PRIVILEGES 0x0
const char osName[] = "Barrelfish";
#endif


#if defined(__linux__)
#elif defined(BARRELFISH)
// Barrelfish
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];
static char *ex_stack_end = ex_stack + EX_STACK_SIZE;

struct bf_mem {
    struct capref  frame;
    void           *vmem;
    struct memobj  *memobj;
    struct vregion *vregion;
    struct capref mapping;
};


static void
bf_alloc_pages(struct bf_mem *bfmem, size_t npages)
{
    errval_t err;
    size_t retfsize;
    const size_t nbytes = npages*BASE_PAGE_SIZE;

    // allocate a frame
    err = frame_alloc(&bfmem->frame, nbytes, &retfsize);
    if (err_is_fail(err)) {
        fprintf(stderr, "frame_alloc: %s\n", err_getstring(err));
        abort();
    }
    assert(retfsize >= nbytes);

    // map frame rw
    err = vspace_map_one_frame_attr_aligned(&bfmem->vmem, retfsize, bfmem->frame,
                                    VREGION_FLAGS_READ_WRITE, LARGE_PAGE_SIZE,
                                    &bfmem->memobj,
                                    &bfmem->vregion);
    if (err_is_fail(err)) {
        fprintf(stderr, "vspace_map: %s\n", err_getstring(err));
        abort();
    }
    struct pmap *pmap = get_current_pmap();
    struct pmap_mapping_info info;
    pmap->f.lookup(pmap, (genvaddr_t)bfmem->vmem, &info);
    bfmem->mapping = info.mapping;
    genvaddr_t mem = (genvaddr_t) bfmem->vmem;
    if (X86_64_PDIR_BASE(mem) != X86_64_PDIR_BASE(mem + retfsize - 1)) {
        debug_printf("WARN: mapping overlaps leaf pt!\n");
    }
}

__attribute__((unused))
static paging_x86_64_flags_t vregion_to_pmap_flag(vregion_flags_t vregion_flags)
{
    paging_x86_64_flags_t pmap_flags =
        PTABLE_USER_SUPERVISOR | PTABLE_EXECUTE_DISABLE;

    if (!(vregion_flags & VREGION_FLAGS_GUARD)) {
        if (vregion_flags & VREGION_FLAGS_WRITE) {
            pmap_flags |= PTABLE_READ_WRITE;
        }
        if (vregion_flags & VREGION_FLAGS_EXECUTE) {
            pmap_flags &= ~PTABLE_EXECUTE_DISABLE;
        }
        if (vregion_flags & VREGION_FLAGS_NOCACHE) {
            pmap_flags |= PTABLE_CACHE_DISABLED;
        }
    }

    return pmap_flags;
}

extern int pmap_selective_flush;
static void
bf_protect(struct bf_mem *bfm, size_t off, size_t len,
           vs_prot_flags_t flags)
{
    //debug_printf("%s: off:%zd len:%zd flags:%u\n", __FUNCTION__, off, len, flags);
    errval_t err;
    size_t pages = len / pagesize;
    // default flush: assisted for single page, full otherwise
    genvaddr_t va_hint = pages == 1 ? (genvaddr_t)bfm->vmem + off : 0;
    pmap_selective_flush = 0;
#if defined(SELECTIVE_FLUSH) && defined(SF_HINT)
    // always do hint-based selective flush
    va_hint = (genvaddr_t)bfm->vmem + off;
    pmap_selective_flush = 2;
#elif defined(SELECTIVE_FLUSH)
    // always do computed selective flush
    va_hint = 1;
    pmap_selective_flush = 1;
#elif defined(FULL_FLUSH)
    // always do full flush
    va_hint = 0;
    pmap_selective_flush = 3;
#endif
#if defined(DIRECT_INVOKE)
    err = invoke_mapping_modify_flags(bfm->mapping, off / pagesize, len / pagesize,
            vregion_to_pmap_flag(flags), va_hint);
#else
    // silence compiler about unused variable va_hint.
    va_hint = va_hint;
    err = bfm->memobj->f.protect(bfm->memobj, bfm->vregion, off, len, flags);
#endif
    if (err_is_fail(err)) {
        fprintf(stderr, "vmpup: memobj.f.protect: %s\n", err_getstring(err));
        abort();
    }

}

struct bf_mem BFmem;

#else
#error "Unknown OS"
#endif

static void handler(int unused) {
    count++;
    if (count >= 0) {
        if (please_protect) {
            #if defined(__linux__)
            mprotect(please_protect, pagesize, NO_PRIVILEGES);
            #elif defined(BARRELFISH)
            assert((uintptr_t)BFmem.vmem <= (uintptr_t)please_protect);
            uintptr_t off_protect = (uintptr_t)please_protect - (uintptr_t)BFmem.vmem;
            bf_protect(&BFmem, off_protect, pagesize, NO_PRIVILEGES);
            #endif
        }

        #if defined(__linux__)
        mprotect(please_unprotect, pagesize, ALL_PRIVILEGES);
        #elif defined(BARRELFISH)
        assert((uintptr_t)BFmem.vmem <= (uintptr_t)please_unprotect);
        uintptr_t off_unprotect = (uintptr_t)please_unprotect - (uintptr_t)BFmem.vmem;
        bf_protect(&BFmem, off_unprotect, pagesize, ALL_PRIVILEGES);
        #endif
    }
}

#if defined(__linux__)
static inline
uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#define debug_printf printf
#endif

#if defined(BARRELFISH)
static void
bf_handler(enum exception_type type, int subtype,
           void *vaddr,
           arch_registers_state_t *regs,
           arch_registers_fpu_state_t *fpuregs)
{
    //debug_printf("got exception %d(%d) on %p\n", type, subtype, vaddr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    handler(0);
}
#endif

int main(int argc, char **argv)
{
    register int i, j, k;

    char *mem;
    #if defined(__linux__)
#ifdef TWO_MB_PAGES
    printf("%s:%s:%d: Use two MiB pages\n", __FILE__, __FUNCTION__, __LINE__);
    pagesize = 2097152;
#else
    pagesize = getpagesize();
#endif
#ifdef HUGETLBFS
    printf("%s:%s:%d: Use hugetlbfs\n", __FILE__, __FUNCTION__, __LINE__);
#define MAP_HUGE_2MB    (21 << MAP_HUGE_SHIFT)
    int flags = MAP_ANONYMOUS | MAP_PRIVATE | MAP_POPULATE;
    if (pagesize == 2097152) {
        flags |= MAP_HUGETLB | MAP_HUGE_2MB;
    }
    
    void* addr = 0x7fa400000000ULL;
    mem = mmap(addr, PAGES*pagesize, PROT_READ | PROT_WRITE, flags, -1, 0);
    if (mem==MAP_FAILED) {
        perror("mmap");
        exit(1);
    }
#else
    if (posix_memalign((void **)&mem, pagesize, PAGES*pagesize) != 0) {
        perror("posix_memalign");
        abort();
    }
#endif
    signal(SIGSEGV, handler);
    #elif defined(BARRELFISH)
    pagesize = BASE_PAGE_SIZE;
    bf_alloc_pages(&BFmem, PAGES);
    mem = BFmem.vmem;
    thread_set_exception_handler(bf_handler, NULL, ex_stack, ex_stack_end, NULL, NULL);
    debug_printf("MEM=%p\n", mem);
#ifdef DIRECT_INVOKE
    debug_printf("USING DIRECT INVOCATIONS\n");
#endif
#ifdef NDEBUG
    debug_printf("ASSERTIONS DISABLED\n");
#endif
    #endif


    for (i = 0; i < PAGES; i++)
        mem[i * pagesize] = 20;

    // NB: perm is global, hence initialized at zero, so perm[0] = 0
    for (i = 1; i < PAGES; i++)
        perm[i] = i;

    // randomly permute pages
    for (i = 1; i < PAGES - 1; i++) {
        j = xrand() % (PAGES - i - 1);
        k = perm[j + i];
        perm[j + i] = perm[i];
        perm[i] = k;
    }

    getrusage(0, &t1);
    //gettimeofday(&tod1, 0);
    ticks1 = rdtsc();
    i = ADDS / 20;
    j = 0;
    do {
        j += k; j += k; j += k; j += k; j += k;
        j += k; j += k; j += k; j += k; j += k;
        j += k; j += k; j += k; j += k; j += k;
        j += k; j += k; j += k;
    } while (--i > 0);

    getrusage(0, &t2);
    //gettimeofday(&tod2, 0);
    ticks2 = rdtsc();

    for (i = 0; i < ROUNDS; i++) {
        #if defined(__linux__)
        mprotect(mem, PAGES * pagesize, NO_PRIVILEGES);
        #elif defined(BARRELFISH)
        bf_protect(&BFmem, 0, PAGES*pagesize, NO_PRIVILEGES);
        #endif
        for (j = 0; j < PAGES; j++) {
            // select next page, and write on it
            please_unprotect = mem + perm[j] * pagesize;
            please_protect = 0;
            *please_unprotect = 10;
        }
    }
    getrusage(0, &t3);
    ticks3 = rdtsc();
    //gettimeofday(&tod3, 0);

    #if defined(__linux__)
    mprotect(mem, PAGES * pagesize, NO_PRIVILEGES);
    mprotect(mem + perm[PAGES - 1] * pagesize, pagesize, ALL_PRIVILEGES);
    #elif defined(BARRELFISH)
    bf_protect(&BFmem, 0, PAGES*pagesize, NO_PRIVILEGES);
    bf_protect(&BFmem, perm[PAGES - 1] * pagesize, pagesize, ALL_PRIVILEGES);
    #endif

    please_protect = mem + perm[PAGES - 1] * pagesize;
    *please_protect = 0;
    getrusage(0, &t4);
    ticks4 = rdtsc();
    //gettimeofday(&tod4, 0);
    for (i = 0; i < ROUNDS; i++)
        for (j = 0; j < PAGES; j++) {
            please_unprotect = mem + perm[j] * pagesize;
            do_nothing(); /* prevents over-optimization here! */
            *please_unprotect = 10;
            please_protect = please_unprotect;
        }
    getrusage(0, &t5);
    ticks5 = rdtsc();
    //gettimeofday(&tod5, 0);

    if (count != 2 * ROUNDS * PAGES)
        printf("Operating system bug, only %d traps instead of %d\n", count,
               2 * ROUNDS * PAGES);
    count = -TRAPS;
    please_unprotect = mem + perm[0] * pagesize;
    while (count < 0) /* this is a loop only to satisfy the 68020, which
                         returns from the trap AFTER the trapping instruction! */
        *please_unprotect = 10;
    getrusage(0, &t6);
    ticks6 = rdtsc();
    gettimeofday(&tod6, 0);

    printf("appel_li: OS:%s\n", osName);
    printf("appel_li: %28s: %6.1f (ticks/page) %d pages\n",
        "prot1+trap+unprot",
        (ticks5-ticks4)/(double)(ROUNDS*PAGES),
        PAGES);

    printf("appel_li: %28s: %6.1f (ticks/page) %d pages\n",
        "protN+trap+unprot",
        (ticks3-ticks2)/(double)(ROUNDS*PAGES),
        PAGES);

    printf("appel_li: %28s: %6.1f (ticks/page) %d traps\n",
        "trap only",
        (ticks6-ticks5)/(double)(TRAPS),
        TRAPS);

    // For harness
    printf("appel_li: done\n");

    /*
    printf("%4.3f & %6.3f & %6.3f & %% %7.1f add\n",
           usrtime(t1, t2) / (double)ADDS, systime(t1, t2) / (double)ADDS,
           tod(tod1, tod2) / (double)ADDS, tod(tod1, tod2) / (double)1e6);
    printf("%4.0f & %6.0f & %6.0f & %% %7.1f prot1+trap+unprot\n",
           usrtime(t4, t5) / (double)(ROUNDS * PAGES),
           systime(t4, t5) / (double)(ROUNDS * PAGES),
           tod(tod4, tod5) / (double)(ROUNDS * PAGES),
           tod(tod4, tod5) / (double)1e6);
    printf("%4.2f & %6.2f & %6.2f & %% %7.1f protN+trap+unprot: %d pages\n",
           usrtime(t2, t3) / (double)(ROUNDS * PAGES),
           systime(t2, t3) / (double)(ROUNDS * PAGES),
           tod(tod2, tod3) / (double)(ROUNDS * PAGES),
           tod(tod2, tod3) / (double)1e6, PAGES);
    printf("%4.2f & %6.2f & %6.2f & %% %7.1f trap only\n",
           usrtime(t5, t6) / (double)TRAPS, systime(t5, t6) / (double)TRAPS,
           tod(tod5, tod6) / (double)TRAPS, tod(tod5, tod6) / (double)1e6);
    printf("%8.2f & %% VM-PUP rating\n",
           3000.0 / ((tod(tod4, tod5) + tod(tod2, tod3) + tod(tod5, tod6)) /
                     (double)(ROUNDS * PAGES)));
    */
    exit(0);
}
