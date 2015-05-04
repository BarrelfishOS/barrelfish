/**
 * \file
 * \brief Test program for large page code
 * 
 * For this to work, Barrelfish needs to be compiled with the
 * PSE == true configuration enabled in hake/config.hs
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <limits.h>

#define SAFE_VADDR (genvaddr_t)0x320000//(8ULL<<18)
#define SAFE_PMAP_ADDR (genvaddr_t)(9ULL<<39)

//4M/2M (in PAE)
#define DEFAULT_SIZE X86_32_LARGE_PAGE_SIZE

    
static vregion_flags_t PMAP_DEFAULT_ACCESS =
    VREGION_FLAGS_READ_WRITE;

static void test_region(uint32_t *buf, size_t size)
{
    int j = 0;
    for (int i = 0; i < size; i+=4) {
        j+=4;
        if( (size-j) < 200)
        {
        printf("%i, ", i);
        }
        
        buf[i/4] = i;
    }
    for (int i = 0; i < size; i+=4) {
        assert(buf[i/4] == i);
    }

    printf("\n\n");
}


int main(void)
{
    struct capref frame, smallframe;
    size_t bytes = DEFAULT_SIZE;
    size_t bits = 4*1024;
    uint32_t address;
    struct memobj a;
    errval_t err;
    printf("\tpagesize: %i, \taddr: %i\n", (int)bytes, (int)SAFE_VADDR);

    //get 4k frame cap
    err = frame_alloc(&smallframe, bits, &bits);
    assert(err_is_ok(err));
    assert(bits >= 4*1024);
    // get 2M/4M frame cap
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= DEFAULT_SIZE);

//test determine_addr as address obtainer
#if 1
    printf("determine_addr 4k\n");
    
    //determine address
    a.size = bits;
    address = 0;
    struct pmap *pmap = get_current_pmap();
    err = pmap->f.determine_addr_raw(pmap, a.size, BASE_PAGE_SIZE, (genvaddr_t*)&address);
    printf("address: %x\n", (unsigned int) address);
    if (err_is_fail(err))
    {
        printf("error in determine_addr: %s\n", err_getstring(err));
        exit(1);
    }
    
    err = pmap->f.map(pmap, address, smallframe, 0, bits, PMAP_DEFAULT_ACCESS, NULL, &bits);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    test_region((uint32_t*)address, bits);
    
    err = pmap->f.unmap(pmap, address, bits, NULL);
    if (err_is_fail(err))
    {
        printf("error in unmap: %s\n", err_getstring(err));
        exit(1);
    } 
#endif

    
// test determine_addr_raw for large pages
// also test unmap on fast path
#if 1
    printf("determine_addr 2m\n");
    bytes = DEFAULT_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= DEFAULT_SIZE);
    
    a.size = bytes;
    printf("a.size: %x, %i\n", (unsigned int) a.size, (unsigned int) a.size);
    address = 0;
    pmap = get_current_pmap();
    err = pmap->f.determine_addr_raw(pmap, a.size, LARGE_PAGE_SIZE, (genvaddr_t*)&address);
    printf("address: %lx\n", (uint32_t) address);
    if(err_is_fail(err))
    {
        printf("error in determine_addr: %s\n", err_getstring(err));
        exit(1);
    }

//for loop to test unmap
for(int i=0; i<10; ++i) {
printf("map %i\n", i);    
    err = pmap->f.map(pmap, address, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    printf("address: %lx\n", (uint32_t) address);
    test_region((uint32_t*)address, DEFAULT_SIZE);
    
    err = pmap->f.unmap(pmap, address, bytes, NULL);
    if (err_is_fail(err))
    {
        printf("error in unmap: %s\n", err_getstring(err));
        exit(1);
    } 
}//endfor
#endif


// test determine_addr_raw for multiple large pages
// also test unmap on slow path
#if 1
    printf("determine_addr 2m multiple\n");
    bytes = 30*DEFAULT_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= 30*DEFAULT_SIZE);
    
    a.size = bytes;
    printf("a.size: %x, %i\n", (unsigned int) a.size, (unsigned int) a.size);
    address = 0;
    pmap = get_current_pmap();
    err = pmap->f.determine_addr_raw(pmap, a.size, LARGE_PAGE_SIZE, (genvaddr_t*)&address);
    printf("address: %lx\n", (uint32_t) address);
    if(err_is_fail(err))
    {
        printf("error in determine_addr: %s\n", err_getstring(err));
        exit(1);
    }

//for loop to test unmap
for(int i=0; i<10; ++i) {
printf("map %i\n", i);    
    err = pmap->f.map(pmap, address, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    printf("address: %lx\n", (uint32_t) address);
    test_region((uint32_t*)address, bytes);
    
    err = pmap->f.unmap(pmap, address, bytes, NULL);
    if (err_is_fail(err))
    {
        printf("error in unmap: %s\n", err_getstring(err));
        exit(1);
    } 
}//endfor
#endif
    printf("exited successfully\n");
    return 0;
}
