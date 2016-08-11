#include <barrelfish/barrelfish.h>
#include <pci/pci.h>

#include "blk_ahci.h"
#include "../blk_debug.h"
#include "ahci_dev.h"

lvaddr_t blk_ahci_get_bar5_vaddr(struct ahci_disk* ad)
{
    assert(ad->bar5->vaddr != NULL);
    return (lvaddr_t)ad->bar5->vaddr;
}

errval_t blk_ahci_init(struct device_mem* bar5, struct ahci_disk** out)
{
    errval_t err;

    err = map_device(bar5);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Map BAR5 failed.");
    }

    struct ahci_disk* ad = calloc(sizeof(struct ahci_disk), 1);
    assert (ad != NULL);

    ad->bar5 = bar5;

    ahci_hba_initialize(&ad->controller, (void *)(bar5->vaddr));
    BLK_DEBUG("Accessing conf regs starting at %p\n", (void *)(bar5->vaddr));
    BLK_DEBUG("Physical address of conf regs: %p\n",  (void *)(bar5->paddr));

    err = ahci_hba_init(&ad->controller);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Init HBA failed.");
    }

    ahci_hba_irq_enable(&ad->controller);

    err = blk_ahci_ports_init(ad);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Port init failed.");
    }

    *out = ad;
    return err;
}


errval_t blk_ahci_stop(struct ahci_disk* ad)
{
    // stop device
    // devmem free (ad->bar5)
    // free (ad)

    return SYS_ERR_OK;
}
