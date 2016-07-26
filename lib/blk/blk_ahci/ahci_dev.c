///! AHCI Device helper functions
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include "blk_ahci.h"
#include "../blk_debug.h"

#include "ahci_dev.h"

size_t ahci_port_offset(uint32_t port)
{
    return 0x100 + port * 0x80;
}

bool ahci_port_is_idle(ahci_port_t* port)
{
    ahci_port_cmd_t cmd = ahci_port_cmd_rd(port);
    uint8_t st = ahci_port_cmd_st_extract(cmd);
    uint8_t cr = ahci_port_cmd_cr_extract(cmd);
    uint8_t fre = ahci_port_cmd_fre_extract(cmd);

    return st == 0 && cr == 0 && fre == 0;
}

bool ahci_port_is_ready(ahci_port_t* port)
{
    uint8_t busy = ahci_port_tfd_bsy_rdf(port);
    uint8_t drq = ahci_port_tfd_drq_rdf(port);
    return busy == 0 && drq == 0;
}

bool ahci_port_is_functional(ahci_port_t* port)
{
    uint8_t det = ahci_port_ssts_det_rdf(port);
    return ahci_port_is_ready(port) && det == 0x3;
}

bool ahci_port_slot_free(ahci_port_t* port, uint8_t slot)
{
    bool ci_free = (ahci_port_ci_rd(port) & (1<<slot)) == 0;
    bool sact_free = (ahci_port_sact_rd(port) & (1<<slot)) == 0;
    return ci_free && sact_free;
}

void ahci_port_start(ahci_port_t* port)
{
    assert(ahci_port_is_functional(port));
    ahci_port_cmd_st_wrf(port, 0x1);
}

void ahci_port_stop(ahci_port_t* port)
{
    ahci_port_cmd_st_wrf(port, 0);

    ahci_port_cmd_t cmd = ahci_port_cmd_rd(port);
    while (ahci_port_cmd_cr_extract(cmd) != 0) {
        // TODO should clear in 500ms
    }

    ahci_port_cmd_fre_wrf(port, 0);
    while (ahci_port_cmd_fr_rdf(port) != 0) {
        // TODO should clear in 500ms
    }
}

bool ahci_port_is_running(ahci_port_t* port)
{
    return ahci_port_cmd_st_rdf(port) > 0;
}

uint32_t ahci_port_probe(ahci_port_t* port)
{
    if (ahci_port_ssts_det_rdf(port) == 0x03) {
        return ahci_port_sig_rd(port);
    }

    return 0;
}

void ahci_hba_reset(ahci_hba_t* controller)
{
    ahci_hba_ghc_t ghc = ahci_hba_ghc_rd(controller);
    BLK_DEBUG("Resetting HBA (setting ghc = %x)...\n", ghc);
    ghc = ahci_hba_ghc_hr_insert(ghc, 1);
    ahci_hba_ghc_wr(controller, ghc);
    barrelfish_usleep(200000);
    while (1) {
        ghc = ahci_hba_ghc_rd(controller);
        if (ahci_hba_ghc_hr_extract(ghc) == 0) {
            BLK_DEBUG("reset done\n");
            break;
        }
    }
}

void ahci_hba_irq_enable(ahci_hba_t* controller)
{
    BLK_DEBUG("Enabling HBA Interrupts\n");
    ahci_hba_ghc_t ghc = ahci_hba_ghc_rd(controller);
    ghc = ahci_hba_ghc_ie_insert(ghc, 1);
    ahci_hba_ghc_wr(controller, ghc);
}

void ahci_hba_irq_disable(ahci_hba_t* controller)
{
    BLK_DEBUG("Disable HBA Interrupts\n");
    ahci_hba_ghc_t ghc = ahci_hba_ghc_rd(controller);
    ghc = ahci_hba_ghc_ie_insert(ghc, 1);
    ahci_hba_ghc_wr(controller, ghc);
}

uint32_t ahci_hba_get_num_ports(ahci_hba_t* controller)
{
    return ahci_hba_cap_np_extract(ahci_hba_cap_rd(controller)) + 1;
}

uint32_t ahci_hba_get_command_slots(ahci_hba_t* controller)
{
    return ahci_hba_cap_ncs_extract(ahci_hba_cap_rd(controller));
}

bool ahci_port_is_implemented(ahci_hba_t* controller, size_t port)
{
    return (ahci_hba_pi_rd(controller) & (1 << port)) > 0;
}

void ahci_port_print_identification(ata_identify_t *id)
{
    char* buf2 = malloc(10*4096);
    assert(buf2 != NULL);
    ata_identify_pr(buf2, 10*4096, id);
    puts(buf2);
    free(buf2);
}

errval_t ahci_hba_init(ahci_hba_t* controller)
{
    ahci_hba_reset(controller);

    // Make sure AHCI is enabled:
    BLK_DEBUG("Setting controller into AHCI mode\n");
    ahci_hba_ghc_t ghc = ahci_hba_ghc_rd(controller);
    ghc = ahci_hba_ghc_ae_insert(ghc, 1);
    ahci_hba_ghc_wr(controller, ghc);

    ahci_hba_irq_disable(controller);

    return SYS_ERR_OK;
}
