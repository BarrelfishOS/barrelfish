#include <barrelfish/barrelfish.h>
#include <barrelfish/vregion.h>
#include <stdio.h>
#include "vspace_dump.h"

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

#define BUFSIZE 8192
void dump_page_tables(void)
{
    struct pmap *pmap = get_current_pmap();
    struct pmap_dump_info *buf = calloc(BUFSIZE, sizeof(struct pmap_dump_info));
    size_t items_written;

    pmap->f.dump(pmap, buf, BUFSIZE, &items_written);

    printf("items_written=%zd\n", items_written);

    qsort(buf, items_written, sizeof(struct pmap_dump_info), cmp_dump_info);

    for (size_t i = 0; i < items_written; i++) {
        struct pmap_dump_info *info = buf+i;
        struct frame_identity fi;
        invoke_frame_identify(info->cap, &fi);
        printf("%zd.%zd.%zd.%zd: 0x%"PRIxGENPADDR", 0x%"PRIxGENVADDR"%s",
                    info->pml4_index,info->pdpt_index,info->pdir_index,info->pt_index,
                    fi.base, info->offset, i % 4 == 3 ? "\n" : "\t");
    }
    printf("\n");

    //puts(buf);
}
