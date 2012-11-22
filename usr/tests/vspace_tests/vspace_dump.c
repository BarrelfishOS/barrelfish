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
#define BUFSIZE (4*1024*1024)
void dump_page_tables(void)
{
    struct pmap *pmap = get_current_pmap();
    char *buf = malloc(BUFSIZE);
    size_t bytes_written;

    pmap->f.dump(pmap, buf, BUFSIZE, &bytes_written);

    printf("bytes_written=%zd\n", bytes_written);

    if (bytes_written == BUFSIZE) {
        buf[BUFSIZE-1] = 0;
    } else {
        buf[bytes_written] = 0;
    }

    puts(buf);
}
