#include <stdio.h>
#include <barrelfish/barrelfish.h>

#define DEBUG_TEST 0

static void test_lookup(struct capref expected_cap, genvaddr_t addr, size_t offset){
    addr += offset;
    errval_t err;
    struct pmap * my_pmap = get_current_pmap();
    assert(my_pmap != NULL);
    struct pmap_mapping_info mi;
    err = my_pmap->f.lookup(my_pmap, addr, &mi);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "pmap lookup");
    }


#if DEBUG_TEST
    printf("=== pmap_mapping_info of 0x%"PRIxGENVADDR" ===\n", addr);
    printf("   vaddr=0x%"PRIxGENVADDR"\n", mi.vaddr);
    printf("   size=%zu\n", mi.size);
    char buf[512];
    debug_print_cap_at_capref(buf, 512, mi.cap);
    printf("   cap = %s\n",buf);
    printf("   offset=0x%" PRIxGENVADDR "\n", mi.offset);
#endif

    assert(capcmp(mi.cap, expected_cap));
    assert(mi.offset + (addr - mi.vaddr) == offset);
}

int main(void){
    printf("Hello world from pmap_test\n");
    
    struct capref frame;
    size_t retsize;
    errval_t err;

    #define SIZE (1024*1024*2)

    err = frame_alloc(&frame, SIZE, &retsize);
    assert(err_is_ok(err));

    struct frame_identity fi;
    err = cap_identify_mappable(frame, &fi);
    
    assert(err_is_ok(err));

    void *va;
    err = vspace_map_one_frame_attr(&va, retsize, frame,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    assert(err_is_ok(err));

    //lookup
    printf("va = %p\n", va);
    struct pmap * my_pmap = get_current_pmap();
    assert(my_pmap != NULL);

#if DEBUG_TEST
    struct pmap_dump_info di[128];
    size_t di_len = 0;
    my_pmap->f.dump(my_pmap,di,128,&di_len);
    for(int i=0; i<di_len;i++){
        printf("pt idx =" PRIfmtPTIDX "", GET_PTIDX(di+i));
        printf("   offset = %"PRIxGENVADDR "\n" , di[i].offset);
        char buf[512];
        debug_print_cap_at_capref(buf, 512, di[i].cap);
        printf("   cap = %s\n",buf);
    }
#endif

    for(size_t off=0; off < SIZE; off += BASE_PAGE_SIZE){ 
        test_lookup(frame, (uintptr_t)va, off);
    }

    printf("pmaplookuptest passed successfully!\n");
    return 0;
}
