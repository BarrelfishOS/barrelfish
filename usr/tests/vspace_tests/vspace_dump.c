#include "vspace_dump.h"

#include <barrelfish/vregion.h>
#include <stdio.h>

void dump_my_vregions(void)
{
    struct vspace *vspace = get_current_vspace();
    struct vregion *walk = vspace->head;

    while (walk != NULL) {
        genvaddr_t base = vregion_get_base_addr(walk);
        genvaddr_t size = vregion_get_size(walk);
        printf("vregion at %"PRIxGENVADDR", size = %"PRIxGENVADDR"\n", base, size);
        walk = walk->next;
    }
}

#ifdef TARGET_X86_64_BARRELFISH_PMAP_H
static int cmp_dump_info(const void *arg1, const void *arg2)
{
    struct pmap_dump_info *info1, *info2;
    info1 = (struct pmap_dump_info *)arg1;
    info2 = (struct pmap_dump_info *)arg2;

    if (info1->pml4_index < info2->pml4_index)
        return -1;
    if (info1->pml4_index > info2->pml4_index)
        return 1;

    // pml indices equal

    if (info1->pdpt_index < info2->pdpt_index)
        return -1;
    if (info1->pdpt_index > info2->pdpt_index)
        return 1;

    // pdpt indices equal

    if (info1->pdir_index < info2->pdir_index)
        return -1;
    if (info1->pdir_index > info2->pdir_index)
        return 1;

    // pdir indices equal

    if (info1->pt_index < info2->pt_index)
        return -1;
    if (info1->pt_index > info2->pt_index)
        return 1;

    // pt indices equal
    return 0;
}
#elif defined(TARGET_X86_32_BARRELFISH_PMAP_H)
static int cmp_dump_info(const void *arg1, const void *arg2)
{
    struct pmap_dump_info *info1, *info2;
    info1 = (struct pmap_dump_info *)arg1;
    info2 = (struct pmap_dump_info *)arg2;

#if CONFIG_PAE
    if (info1->pdpt_index < info2->pdpt_index)
        return -1;
    if (info1->pdpt_index > info2->pdpt_index)
        return 1;

    // pdpt indices equal
#endif

    if (info1->pdir_index < info2->pdir_index)
        return -1;
    if (info1->pdir_index > info2->pdir_index)
        return 1;

    // pdir indices equal

    if (info1->pt_index < info2->pt_index)
        return -1;
    if (info1->pt_index > info2->pt_index)
        return 1;

    // pt indices equal
    return 0;
}
#else
static int cmp_dump_info(const void *arg1, const void *arg2)
{
	return 0;
}
#endif

#define BUFSIZE 8192
void dump_pmap(struct pmap *pmap)
{
    errval_t err;

    struct pmap_dump_info *buf = calloc(BUFSIZE, sizeof(struct pmap_dump_info));
    size_t items_written;

    pmap->f.dump(pmap, buf, BUFSIZE, &items_written);

    printf("items_written=%zd\n", items_written);

    qsort(buf, items_written, sizeof(struct pmap_dump_info), cmp_dump_info);

    for (size_t i = 0; i < items_written; i++) {
        struct pmap_dump_info *info = buf+i;
        struct frame_identity fi;
        err = frame_identify(info->cap, &fi);
        assert(err_is_ok(err));
        printf(PRIfmtPTIDX": 0x%"PRIxGENPADDR", 0x%"PRIxGENVADDR", 0x%zx\n",
                    GET_PTIDX(info),
                    fi.base, info->offset, fi.bytes);
    }
    printf("\n");

    //puts(buf);
}

void dump_page_tables(void)
{
    struct pmap *pmap = get_current_pmap();

    dump_pmap(pmap);
}
