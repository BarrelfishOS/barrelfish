/**
 * \file
 * \brief Test program for large page code
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

#define SAFE_VADDR (genvaddr_t)(8ULL<<39)
#define SAFE_PMAP_ADDR (genvaddr_t)(9ULL<<39)
// 2M
#define DEFAULT_SIZE X86_64_LARGE_PAGE_SIZE

static enum objtype type[] = {
    ObjType_VNode_x86_64_pml4,
    ObjType_VNode_x86_64_pdpt,
    ObjType_VNode_x86_64_pdir,
    ObjType_VNode_x86_64_ptable
};

// the offsets of the indices for the different page table levels
static uint8_t offsets[] = { 39, 30, 21, 12 };

static paging_x86_64_flags_t PAGE_DEFAULT_ACCESS =
    PTABLE_USER_SUPERVISOR |
    PTABLE_EXECUTE_DISABLE |
    PTABLE_READ_WRITE;
    
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
    struct capref frame, smallframe, vnodes[4];
    size_t bytes = DEFAULT_SIZE;
    size_t bits = 4*1024;
    errval_t err;

    //get 4k frame cap
    err = frame_alloc(&smallframe, bits, &bits);
    assert(err_is_ok(err));
    assert(bits >= 4*1024);
    // get 2M frame cap
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= DEFAULT_SIZE);

    // check that we have the necessary ptables (note: this is not how you
    // should do this cf. with pmap_target.c
    // setup reference to pml4 capability
    vnodes[0] = (struct capref) {
        .cnode = cnode_page,
        .slot  = 0,
    };
    // note we start iterating on 1, because we're using index 0 as the
    // well-known pml4 capref
    for (int i = 1; i < sizeof(type) / sizeof(type[0]); i++) {
        err = slot_alloc(&vnodes[i]);
        assert(err_is_ok(err));
        printf("creating vnode for level %d, type %d\n", i, type[i]);
        err = vnode_create(vnodes[i], type[i]);
        assert(err_is_ok(err));
        uint32_t slot = (SAFE_VADDR >> offsets[i-1]) & 0x1f;
        printf("mapping into slot %d on level %d\n", slot, i-1);
        err = vnode_map(vnodes[i-1], vnodes[i], slot, PTABLE_ACCESS_DEFAULT, 0, 1);
        if (err_is_fail(err)) {
            // this means we already have a page table for this level
            // XXX: right now we've chosen the SAFE_VADDR such that we don't have
            // any intermediate level page tables so we don't need to worry
            // about this case
            printf("there was a page table already?\n");
            printf("vnode_map: %s\n", err_getstring(err));
            exit(1);
        }
    }
    
// map as 4k and 2m frames with vnode_map
// used to test the kernel code
#if 1
    //printf("start 4k vnode map");
    err = vnode_map(vnodes[3], smallframe, (SAFE_VADDR>>offsets[3])&0x1f,
            PAGE_DEFAULT_ACCESS, 0, 4*1024 / X86_64_BASE_PAGE_SIZE);
#endif
#if 0
    if (err_is_fail(err)) {
        printf("error in vnode_map: %s\n", err_getstring(err));
        exit(1);
    }

    test_region((uint32_t*)SAFE_VADDR, 4*1024);

    // FROM HERE: unmap and try to map as large page

    // unmap frame
    printf("start 4k unmap\n");
    err = vnode_unmap(vnodes[3], smallframe, (SAFE_VADDR>>offsets[3])&0x1f,
            4*1024 / X86_64_BASE_PAGE_SIZE);
    if (err_is_fail(err)) {
        printf("vnode_unmap: %s\n", err_getstring(err));
    }
    assert(err_is_ok(err));
    // unmap level 3 page table
    err = vnode_unmap(vnodes[2], vnodes[3], SAFE_VADDR>>offsets[2]&0x1f, 1);
    assert(err_is_ok(err));
    
    // map as 2M large page
    printf("start 2m vnodemap\n");
    err = vnode_map(vnodes[2], frame, SAFE_VADDR>>offsets[2]&0x1f,
            PAGE_DEFAULT_ACCESS, 0, DEFAULT_SIZE / X86_64_LARGE_PAGE_SIZE);
    if (err_is_fail(err)) {
        printf("error in vnode_map: %s\n", err_getstring(err));
        exit(1);
    }

    test_region((uint32_t*)SAFE_VADDR, DEFAULT_SIZE);
    
    err = vnode_unmap(vnodes[2], frame, SAFE_VADDR>>offsets[2]&0x1f, DEFAULT_SIZE / X86_64_LARGE_PAGE_SIZE);
    if (err_is_fail(err)) {
        printf("vnode_unmap: %s\n", err_getstring(err));
    }
    assert(err_is_ok(err));
#endif
    
    struct pmap *pmap;
    
//normal page via pmap interface
// used to test if 4k code still works
// (although this would break the whole barrelfish boot)
#if 0
    printf("\n\nstart 4k map with pmap\n");
    bits = 4*1024;
    printf("    frame_alloc\n");
    err = frame_alloc(&smallframe, bits, &bits);
    assert(err_is_ok(err));
    assert(bits >= 4*1024);
    printf("    get pmap\n");
    pmap = get_current_pmap();
    
    
    printf("    map\n");
    err = pmap->f.map(pmap, SAFE_PMAP_ADDR, smallframe, 0, bits, PMAP_DEFAULT_ACCESS, NULL, &bits);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
    }
    test_region((uint32_t*)SAFE_PMAP_ADDR, 4*1024);
    
    printf("\tunmap\n");
    err = pmap->f.unmap(pmap, SAFE_PMAP_ADDR, bits, NULL);
#endif

    
//large page via pmap interface
// used to test the 2M pages on a safe address
// looped 10 times to see if unmap does work
#if 0
    printf("start 2m map with pmap\n");
    bytes = DEFAULT_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= DEFAULT_SIZE);
    
    pmap = get_current_pmap();

    for(int i = 0; i<10; ++i){
        printf("map %i\n", i);
        err = pmap->f.map(pmap, SAFE_PMAP_ADDR, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
        if (err_is_fail(err))
        {
            printf("error in pmap: %s\n", err_getstring(err));
            exit(1);
        }
    
        test_region((uint32_t*)SAFE_PMAP_ADDR, DEFAULT_SIZE);
    
        err = pmap->f.unmap(pmap, SAFE_PMAP_ADDR, bytes, NULL);
        if (err_is_fail(err))
        {
            printf("error in unmap: %s\n", err_getstring(err));
            exit(1);
        } 
    }//end for
#endif


        struct memobj a;
        genvaddr_t address;
        
//test determine_addr_raw as address obtainer
// mapping 4k page
#if 0
    printf("determine_addr 4k\n");
    bits = 4*1024;
    err = frame_alloc(&smallframe, bits, &bits);
    assert(err_is_ok(err));
    assert(bits >= 4*1024);
    
    //determine address
    a.size = bits;
    address = 0;
    pmap = get_current_pmap();
    err = pmap->f.determine_addr_raw(pmap, a.size, BASE_PAGE_SIZE, &address);
    printf("address: %lx\n", (unsigned long) address);
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
    test_region((uint32_t*)address, 4*1024);
#endif


// determine_addr_raw for 2m pages
// for loop inserted currently
// this was the only way multiple large pages could be accessed
// no longer necessary
#if 0
    printf("determine_addr 2m\n");
    genvaddr_t* addresses = malloc(200*sizeof(genvaddr_t));
for(int i = 0; i<200; ++i){
    bytes = DEFAULT_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= DEFAULT_SIZE);
    
    a.size = bytes;
    printf("a.size: %x, %i\n", (unsigned int) a.size, (unsigned int) a.size);
    address = 0;
    pmap = get_current_pmap();
    err = pmap->f.determine_addr_raw(pmap, a.size, LARGE_PAGE_SIZE, &address);
    printf("address: %x\n", (unsigned int) address);
    if(err_is_fail(err))
    {
        printf("error in determine_addr: %s\n", err_getstring(err));
        exit(1);
    }
    
    err = pmap->f.map(pmap, address, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    addresses[i] = address;
}
for(int i = 0; i<200; ++i){
    test_region((uint32_t*)addresses[i], DEFAULT_SIZE);
}
#endif

    
// multiple large pages with one go
// the for loop is to test unmap
#if 1
    printf("multiple 2m\n");
    bytes = 10*DEFAULT_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    assert(err_is_ok(err));
    assert(bytes >= 10*DEFAULT_SIZE);
    
    a.size = bytes;
    printf("a.size: %x, %i\n", (unsigned int) a.size, (unsigned int) a.size);
    address = 0;
    pmap = get_current_pmap();
    err = pmap->f.determine_addr_raw(pmap, a.size, LARGE_PAGE_SIZE, &address);
    printf("address: %x\n", (unsigned int) address);
    if(err_is_fail(err))
    {
        printf("error in determine_addr: %s\n", err_getstring(err));
        exit(1);
    }
for (int i=0; i<10; ++i) {  
printf("map %i\n", i);  
    err = pmap->f.map(pmap, address, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }

    test_region((uint32_t*)address, 10*DEFAULT_SIZE);

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
