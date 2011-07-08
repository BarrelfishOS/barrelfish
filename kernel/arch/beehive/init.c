/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <serial.h>
#include <stdio.h>
#include <string.h> // for memmove
#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/paging_arch.h> // for BASE_PAGE_SIZE
#include <target/beehive/barrelfish_kpi/coredata_target.h>
#include <corearea.h>
#include <simctrl.h>
#include <dcache.h>
#include "beekernel.h"
#include "bmp.h"

// <This section should be in a arch header file>

#define MAX_IMAGES 32
bexec_t *MovedImages[MAX_IMAGES];
int MovedImageCount;

#define ROUNDUP(x,y) (((x)+(y)-1) & ~((y)-1))

extern void mmu_add_entry(uint32_t start, uint32_t length, int read, int write);

// </This section should be in a arch header file>

extern void ringt0(uint32_t *ptr, int count);
extern void ringt1(uint32_t *ptr, int count);
extern void ringt2(uint32_t *ptr, int count);
extern void ringt3(uint32_t *ptr, int count);
extern void ringt4(uint32_t *ptr, int count);

uint32_t array[6*10];

// On most systems this is called _end, but on beehive it is a
// word/code address, and must be shifted for data accesses.
// care that some of these are Linker absolute and dont relocate
// properly to other than the first image.
extern void _code_ibefore(void);
extern void _code_iafter(void);
extern void _data_ibefore(void);
extern void _data_iafter(void);

int bmain(struct beehive_core_data *bcd);

static void MoveImages(void**const pinitaddr, pstringtable_t *const ppstrings);

// findbits.S

extern int find_least_clear32(uint32_t value);
//extern int find_least_set32(uint32_t value);
extern int find_least_clear64(uint64_t value);
extern int find_least_set64(uint64_t value);
extern int find_highest_clear32(uint32_t value);
//extern int find_highest_set32(uint32_t value);
extern int find_highest_clear64(uint64_t value);
extern int find_highest_set64(uint64_t value);
extern int population_count(uint32_t value);

// startup.c

extern void kernel_bsp_startup(void* initaddr, pstringtable_t strings);
extern void kernel_app_startup(struct beehive_core_data *bcd);
extern void kernel_alloc_init(lpaddr_t base, lpaddr_t limit);

// Tell startup.c where it can start allocating memory

/**
 * \brief Test if this is the bootstrap processor
 *
 */
inline bool arch_core_is_bsp(void)
{
    int core = arch_get_core_id();
    return core == 1 || core == 2;
}

/*
 * the bcd pointer is only valid if we are not the boot core
 */
int bmain(struct beehive_core_data *bcd)
{
    uint8_t core;

    core = arch_get_core_id();
    if (core > 1) {
	BEE_SIMCTRL(BEE_SIMCTRL_REGISTERS);
    }

    // do not remove/change this printf: needed by regression harness
    printf("Barrelfish CPU driver starting on Beehive core %u\n", core);

    mmu_add_entry((uint32_t)_code_ibefore,
		  (((uint32_t)_data_ibefore) >> 2) - (uint32_t)_code_ibefore,
		  1, 0);

    // Write system call vector into this core's core area
    struct corearea *corearea = my_corearea();

    extern void systrap(void); // assembler function
    corearea->syscall = (uint32_t)systrap;
    printf("Set system call entry at %p to %#x\n",
	   &corearea->syscall, corearea->syscall);

    extern void interrupt(void); // assembler function
    corearea->kernel_ticker = (uint32_t)interrupt;
    printf("Set kernel ticker entry at %p to %#x\n",
	   &corearea->kernel_ticker, corearea->kernel_ticker);

    corearea->kernel_begins = (uint32_t)_code_ibefore;
    corearea->kernel_ends = (uint32_t)(_data_ibefore) >> 2; // because of linker
    printf("Set kernel begins %#x and ends %#x\n",
	   corearea->kernel_begins, corearea->kernel_ends);

    // Tests

    extern int lli_test(void);
    int lli_result = lli_test();
    if (lli_result == 1)
	printf("lli_test(): passed ok\n");
    else
	printf("\n**** lli_test(): failed with code %#x\n\n", lli_result);

    extern unsigned skip_test(unsigned t); // ~20 cycles per iteration
    unsigned skip_result = skip_test(arch_is_simulator() ? 100000 : 10000000);
    if (skip_result != 0)
	panic("skip_test: %#x\n", skip_result);


    // TODO: Might need to move bmp init to before first printf for real HW
    bmp_init();
    if (arch_core_is_bsp()) {
#if 0
	// XXX HACK
	if (core == 2) {
	    ringt1(array, 10);
	    for(int i=0; i<60; i+=6) {
		printf("%8u %8u %8u %8u %8u %8u\n",
		       array[i], array[i+1], array[i+2],
		       array[i+3], array[i+4], array[i+5]);
	    }
	}
#endif
        //kernel_startup_early();
        void *initaddr;
        pstringtable_t strings;
        MoveImages(&initaddr, &strings);
        bee_dcache_flush_all();
	if (!strings)
	    panic("no string table found");
        kernel_bsp_startup(initaddr, strings);
    } else {
        printf("core_data at %p\n", bcd);
        printf("cpu_module at %p\n", bcd->cpu_module);
        printf("monitor_module at %p\n", bcd->monitor_module);
        kernel_alloc_init(bcd->memory_base, bcd->memory_limit);
        kernel_app_startup(bcd);
    }

    return 0; // "error: return type of main is not int"
} // bmain


static unsigned int ImageSum(const unsigned int *ptr, size_t count)
{
    unsigned int val = 0;
    do
	val += *(ptr++);
    while(--count > 0);
    return val ^ ~((val << 1) | (val >> 31));
}


static void MoveImages(void**const pinitaddr, pstringtable_t *const ppstrings)
{
    // current last position in word addresses
    //    uintptr_t codelast = (uintptr_t)0x1000; 
    uintptr_t codelast = (uintptr_t)&_data_iafter;
    bexec_t *bexec = (void*)(codelast << 2);
    pstringtable_t strings = NULL;
    pstringtable_t strings2 = NULL;
    printf("_data_iafter = %p\n", &_data_iafter);
    printf("bexec = %p\n", bexec);

    MovedImageCount = 0;
    while (MovedImageCount < MAX_IMAGES) {
	if ((bexec->bmagic == BEXEC_BMAGIC)
	    && (bexec->btorg >= codelast) && (bexec->btsize > 8)
	    && (bexec->bdorg >= bexec->btorg + bexec->btsize)
	    && (bexec->bborg >= bexec->bdorg + bexec->bdsize)
	    && (ImageSum((void*)bexec, bexec->btsize + bexec->bdsize) == 0)) {
	    // Found an image
	    printf("Found image %d at %p [%#x %#x %#x %#x %#x %#x %#x]\n", 
                   MovedImageCount, bexec, bexec->bmagic,
		   bexec->btorg, bexec->btsize, bexec->bdorg, bexec->bdsize,
		   bexec->bborg, bexec->bbsize);
	    MovedImages[MovedImageCount++] = bexec;
	    codelast = ROUNDUP(bexec->bborg + bexec->bbsize, (BASE_PAGE_SIZE >> 2));
	    bexec = (void*)(((uintptr_t)bexec) + ((bexec->btsize + bexec->bdsize) << 2));
            // Check that we're not exceeding MAX_IMAGES
            if (MovedImageCount == MAX_IMAGES) {
              panic("reached MAX_IMAGES before end of input");
            }
	} else if (bexec->bmagic == BEXEC_ARGSMAGIC) {
	    strings = (pstringtable_t)bexec;
 	    strings2 = (pstringtable_t)(codelast << 2);
	    printf("Found strings area at %p size %u move to %p\n",
		   strings, strings->slength, strings2);
	    codelast += (ROUNDUP(strings->slength, BASE_PAGE_SIZE)) >> 2;
	    break; // string table is last
	} else {
	    printf ("No image at %p [%#x %#x %#x %#x %#x %#x %#x]\n", 
                   bexec, bexec->bmagic,
		   bexec->btorg, bexec->btsize, bexec->bdorg, bexec->bdsize,
		   bexec->bborg, bexec->bbsize);
	    break;
	}
    }
    printf("Found %d images total\n", MovedImageCount);

    printf("codelast=%#x\n", codelast);

    if (strings != NULL) {
	memmove(strings2, strings, strings->slength);
    }

    // Now they are found, move each one into place and zap its bss
    for (int i=MovedImageCount-1; i>=0; i--) {
	bexec = MovedImages[i];
	// Remember to move backwards down memory
	void *src = bexec;
	void *dst = (void*)(bexec->btorg << 2);
	size_t len = (bexec->bdorg + bexec->bdsize - bexec->btorg) << 2;

	// Must memset before memmove since bexec->... can be different
	// after the memmove!
        size_t zlen = ((bexec->bborg+bexec->bbsize)
                       - (bexec->bdorg+bexec->bdsize));
        void *edata = (void*)((bexec->bdorg + bexec->bdsize)<<2);
        memset(edata, 0, zlen << 2);

        if (((uintptr_t)src^(uintptr_t)dst) & (sizeof(uintptr_t)-1)) {
            printf("warning: slow unaligned memmove\n");
        }
	memmove(dst, src, len);
	MovedImages[i] = dst;
        bexec = dst;
#if 0
        printf("Checksum is now %#x\n", 
               ImageSum((void*)bexec, bexec->btsize + bexec->bdsize));
        printf("Checksum (w/BSS) is now %#x\n", 
               ImageSum((void*)bexec, bexec->btsize + bexec->bdsize + bexec->bbsize));
#endif

	// And protect it
        printf("Write-protect TEXT %#x->%#x\n", bexec->btorg, bexec->btorg+bexec->btsize);
	mmu_add_entry(bexec->btorg, bexec->btsize, 1, 0);	
        if (i < 1) continue;
        continue;
        printf("Write-protect DATA %#x->%#x\n", bexec->bdorg, bexec->bdorg+bexec->bdsize);
        mmu_add_entry(bexec->bdorg, bexec->bdsize, 1, 0);

        printf("Write-protect BSS %#x->%#x\n", bexec->bborg, bexec->bborg+bexec->bbsize);
        mmu_add_entry(bexec->bborg, bexec->bbsize, 1, 0);

    }

    printf("All images moved\n");

    // First free address
    uintptr_t location = codelast << 2;

    // Roundup to a sane granularuity 
    printf("location = %#x\n", location);
    location = ROUNDUP(location, BASE_PAGE_SIZE);
    kernel_alloc_init(location, location + BASE_PAGE_SIZE*1024);
   
    // Results
    *pinitaddr = (MovedImageCount > 0 ? MovedImages[0] : NULL);
    *ppstrings = strings2;
} // MoveImages
