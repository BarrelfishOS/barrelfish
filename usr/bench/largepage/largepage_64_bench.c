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
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

#define SAFE_VADDR (genvaddr_t)(8ULL<<39)
#define SAFE_PMAP_ADDR (genvaddr_t)(9ULL<<39)
//0b 0000 0000 0000 0000 0000 0100 1000 0000 0000 0000 0000 0000 0000 0000 0000 0000
#define SAFE_PMAP_ADDR_L ((genvaddr_t) (12ULL<<39))
// 40M
#define DEFAULT_SIZE 30*X86_64_LARGE_PAGE_SIZE

#define LARGE_PAGE_SIZE X86_64_LARGE_PAGE_SIZE

#define RUN_COUNT 50

unsigned long mean4k;
unsigned long mean2m;
unsigned long min4k;
unsigned long min2m;
unsigned long mean4krand;
unsigned long mean2mrand;
unsigned long min4krand;
unsigned long min2mrand;
unsigned long max4krand;
unsigned long max2mrand;
unsigned long max4k;
unsigned long max2m;

unsigned long mean4kshort;
unsigned long mean2mshort;
unsigned long mean4kshortrand;
unsigned long mean2mshortrand;
unsigned long min4kshort;
unsigned long min2mshort;
unsigned long min4kshortrand;
unsigned long min2mshortrand;
unsigned long max4kshort;
unsigned long max2mshort;
unsigned long max4kshortrand;
unsigned long max2mshortrand;
/*static paging_x86_64_flags_t PAGE_DEFAULT_ACCESS =
    PTABLE_USER_SUPERVISOR |
    PTABLE_EXECUTE_DISABLE |
    PTABLE_READ_WRITE;
 */   
static vregion_flags_t PMAP_DEFAULT_ACCESS =
    VREGION_FLAGS_READ_WRITE;

static void test_region(uint32_t *buf, size_t size, bool large, bool area)
{
    cycles_t runs[RUN_COUNT];
    cycles_t start, end;
    uint64_t mean = 0;
    unsigned long min = 0;
    unsigned long max = 0;
    printf("base address: %lx\n", (uint64_t) buf);
    
    //sequential access
    for (int j = 0; j < RUN_COUNT; j++) {
        
        start = bench_tsc()/1000;
        for (unsigned long i = 0; i < size; i+=4) {
            
            buf[i/4] = i;
        }
        for (unsigned long i = 0; i < size; i+=4) {
            assert(buf[i/4] == i);
        }
        end = bench_tsc()/1000;
        runs[j] = end-start;
        mean += (unsigned int) runs[j];
        if (runs[j] < min || j == 0) {min = runs[j];}
        if (runs[j] > max) {max = runs[j];}
        printf("run: %u, cycles: %u\n", j, (unsigned int) runs[j]);
    }
    mean = mean/RUN_COUNT;
    printf("\naverage cycles to write the whole area: %u\n", (unsigned int) mean);
    if(!large && !area) {
        mean4k = mean;
        min4k = min;
        max4k = max;
    }
    else if (large && !area) {
        mean2m = mean;
        min2m = min;
        max2m = max;
    }
    else if (!large && area) {
        mean4kshort = mean;
        min4kshort = min;
        max4kshort = max;
    } else {
        mean2mshort = mean;
        min2mshort = min;
        max2mshort = max;
    }
    
    //random access
    //generate 1M random number array
    unsigned int* addrs;
    min = 0;
    max = 0;
    mean = 0;
    addrs = malloc(2000000*sizeof(unsigned int));
    printf("malloc'd\n");
    for (int i = 0; i<2000000; i++)
    {  
        addrs[i] = (rand() % (size/4));
    }
    printf("randomised\n");
    for (int j = 0; j < RUN_COUNT; j++) {
        
        start = bench_tsc()/1000;
        for (int i = 0; i < 2000000; i++) {
            buf[addrs[i]] = addrs[i];
        }
        for (unsigned long i = 0; i < 2000000; i++) {
            assert(buf[addrs[i]] == addrs[i]);
        }
        end = bench_tsc()/1000;
        runs[j] = end-start;
        mean += (unsigned int) runs[j];
        if (runs[j] < min || j == 0) {min = runs[j];}
        if (runs[j] > max) {max = runs[j];}
        printf("run: %u, cycles: %u\n", j, (unsigned int) runs[j]);
    }
    mean = mean/RUN_COUNT;
    printf("\naverage cycles to write the whole area randomly: %u\n", (unsigned int) mean);
    if(!large && !area) {
        mean4krand = mean;
        min4krand = min;
        max4krand = max;
    }
    else if (large && !area){
        mean2mrand = mean;
        min2mrand = min;
        max2mrand = max;
    }else if (!large && area) {
        mean4kshortrand = mean;
        min4kshortrand = min;
        max4kshortrand = max;
    } else {
        mean2mshortrand = mean;
        min2mshortrand = min;
        max2mshortrand = max;
    }

}

int main(void)
{
    struct capref frame;
    size_t bytes = DEFAULT_SIZE;
    errval_t err;
    mean4k = 0;
    mean2m = 0;
    genvaddr_t address;

    
    //normal pages via pmap interface
    printf("\nstart 4k map with pmap\n");
    err = frame_alloc(&frame, bytes, &bytes);
    if (err_is_fail(err))
    {  
        printf("error in frame_alloc: %s\n", err_getstring(err));
        exit(1);
    }
    assert(bytes >= DEFAULT_SIZE);
    printf("    get pmap\n");
    struct pmap *pmap = get_current_pmap();
    
    printf("    obtain address\n");
    err = pmap->f.determine_addr_raw(pmap, bytes, X86_64_BASE_PAGE_SIZE, &address);
    if (err_is_fail(err))
    {
        printf("error in determine_addr_raw: %s\n", err_getstring(err));
        exit(1);
    }
    
    printf("    map\n");
    err = pmap->f.map(pmap, address, frame, 0, bytes, PMAP_DEFAULT_ACCESS, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    printf("addr: %lx\n", address);
    test_region((uint32_t*)address, DEFAULT_SIZE, false, false);
    
    printf("\tunmap\n");
    err = pmap->f.unmap(pmap, address, bytes, NULL);
    if (err_is_fail(err))
    {
        printf("error in unmap: %s\n", err_getstring(err));
        exit(1);
    }
   
    //large page via pmap interface
    printf("start 2m map with pmap\n");
    bytes = DEFAULT_SIZE;
    struct capref frame2;
    err = frame_alloc(&frame2, bytes, &bytes);
    if (err_is_fail(err))
    {  
        printf("error in frame_alloc: %s\n", err_getstring(err));
        exit(1);
    }
    assert(bytes >= DEFAULT_SIZE);
    pmap = get_current_pmap();

    printf("determine address\n");
    err = pmap->f.determine_addr_raw(pmap, bytes, X86_64_LARGE_PAGE_SIZE, &address);
    if (err_is_fail(err))
    {
        printf("error in determine_addr_raw: %s\n", err_getstring(err));
        exit(1);
    }
    
    printf("map\n");
    err = pmap->f.map(pmap, address, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    printf("addr: %lx\n", address);
    test_region((uint32_t*)address, DEFAULT_SIZE, true, false);
    
    err = pmap->f.unmap(pmap, address, bytes, NULL);
    if (err_is_fail(err))
    {
        printf("error in unmap: %s\n", err_getstring(err));
        exit(1);
    }
    //small area 4k
    bytes = LARGE_PAGE_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    if (err_is_fail(err))
    {  
        printf("error in frame_alloc: %s\n", err_getstring(err));
        exit(1);
    }
    assert(bytes >= LARGE_PAGE_SIZE);
    pmap = get_current_pmap();
    err = pmap->f.map(pmap, SAFE_PMAP_ADDR, frame, 0, bytes, PMAP_DEFAULT_ACCESS, NULL, &bytes);
    if (err_is_fail(err))
    { 
        printf("error in pmap small 4k\n");
        exit(1);
    }
    test_region((uint32_t*) SAFE_PMAP_ADDR, LARGE_PAGE_SIZE, false, true); 
    
    //small area 2m
    bytes = LARGE_PAGE_SIZE;
    err = frame_alloc(&frame, bytes, &bytes);
    if (err_is_fail(err))
    {  
        printf("error in frame_alloc: %s\n", err_getstring(err));
        exit(1);
    }
    assert(bytes >= LARGE_PAGE_SIZE);
    pmap = get_current_pmap();

    printf("map\n");
    err = pmap->f.map(pmap, SAFE_PMAP_ADDR_L, frame, 0, bytes, PMAP_DEFAULT_ACCESS | 0x0100, NULL, &bytes);
    if (err_is_fail(err))
    {
        printf("error in pmap: %s\n", err_getstring(err));
        exit(1);
    }
    printf("addr: %lx\n", SAFE_PMAP_ADDR_L);
    test_region((uint32_t*)SAFE_PMAP_ADDR_L, LARGE_PAGE_SIZE, true, true);
    
    
        
    printf("large area\n");
    printf("average 4k: %lu, average 2m: %lu\n", mean4k, mean2m);
    printf("minimal 4k: %lu, minimal 2m: %lu\n", min4k, min2m);
    printf("maximal 4k: %lu, maximal 2m: %lu\n", max4k, max2m);
    printf("random: average 4k: %lu, average 2m: %lu\n", mean4krand, mean2mrand);
    printf("random:minimal 4k: %lu, minimal 2m: %lu\n", min4krand, min2mrand);
    printf("random:maximal 4k: %lu, maximal 2m: %lu\n\n", max4krand, max2mrand);
    printf("short area\n");
    printf("average 4k: %lu, average 2m: %lu\n", mean4kshort, mean2mshort);
    printf("minimal 4k: %lu, minimal 2m: %lu\n", min4kshort, min2mshort);
    printf("maximal 4k: %lu, maximal 2m: %lu\n", max4kshort, max2mshort);
    printf("random: average 4k: %lu, average 2m: %lu\n", mean4kshortrand, mean2mshortrand);
    printf("random:minimal 4k: %lu, minimal 2m: %lu\n", min4kshortrand, min2mshortrand);
    printf("random:maximal 4k: %lu, maximal 2m: %lu\n", max4kshortrand, max2mshortrand);
    printf("exited successfully\n");
    return 0;
}
