/**
 * Program that maps its own page-table into
 * it's virtual address space.
 **/
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

static void print_vnodes(struct vnode* current, int depth) {

    char pad[depth+1];
    for (int i=0; i<depth; i++) {
        pad[i] = ' ';
    }
    pad[depth] = '\0';

    char capbuffer[1024];

    while(current != NULL) {
        printf("%s", pad);
        printf("vnode:%p { entry: %d is_vnode: %d\n", current, current->entry, current->is_vnode);
        if (current->is_vnode) {
            debug_print_cap_at_capref(capbuffer, 1024, current->u.vnode.cap);
            printf("%s", pad);
            printf("vnode: cap=%s\n", capbuffer);
            print_vnodes(current->u.vnode.children, depth+1);
        }
        else {
            debug_print_cap_at_capref(capbuffer, 1024, current->u.frame.cap);
            printf("%s", pad);
            printf("frame: cap=%s offset:%"PRIxGENVADDR" flags:%d", capbuffer, current->u.frame.offset, current->u.frame.flags);
        }
        printf("\n");
        printf("%s", pad);
        printf("}\n");
        current = current->next;
    }
}

int main(int argc, char *argv[])
{
    errval_t err;

    printf("%s:%s:%d: Hi, lets map our own ptable!\n", 
           __FILE__, __FUNCTION__, __LINE__);

    struct pmap* pmap = get_current_pmap();
    assert(pmap != NULL);
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;

    printf("%s:%s:%d: About to print the vnode tree\n", 
           __FILE__, __FUNCTION__, __LINE__);
    struct vnode* current = &x86->root;
    print_vnodes(current, 0);

    current = &x86->root;
    char capbuffer[1024];
    
    struct capref pml4 = current->u.frame.cap;
    debug_print_cap_at_capref(capbuffer, 1024, pml4);
    printf("%s\n", capbuffer);

    struct frame_identity id = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(pml4, &id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Invoke frame identify failed.");
    }
    printf("%s:%s:%d: pml4.bits = %d\n", 
           __FILE__, __FUNCTION__, __LINE__, id.bits);

    void *retaddr = NULL;
    err = vspace_map_one_frame(&retaddr, (size_t)1<<id.bits, pml4, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Can not map PML4 table.");
    }

    lvaddr_t* ptable = (lvaddr_t*)retaddr;
    printf("retaddr[0] = 0x%"PRIxGENPADDR"\n", ptable[0]);
    printf("retaddr[1] = 0x%"PRIxGENPADDR"\n", ptable[1]);

//    return vspace_map_one_frame_attr(retaddr, size, frame,
//                                     VREGION_FLAGS_READ_WRITE, retmemobj,
//                                     retvregion);


    //printf("%s:%s:%d: pmap = %p\n", __FILE__, __FUNCTION__, __LINE__, pmap);
    return EXIT_SUCCESS;
}
