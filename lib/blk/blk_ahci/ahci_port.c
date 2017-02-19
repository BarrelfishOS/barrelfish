#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <pci/pci.h>

#include "blk_ahci.h"
#include "ahci_dev.h"
#include "sata_fis.h"

#include "../blk_debug.h"
#include "../dma_mem/dma_mem.h"

static ahci_port_chdr_t get_command_header(struct ahci_port* port, uint8_t slot)
{
    return (ahci_port_chdr_t) ((uint8_t*)port->clb.vaddr) + (ahci_port_chdr_size * slot);
}

static struct region_descriptor region_descriptor_new(uint64_t dba, uint32_t dbc, bool irq)
{
    struct region_descriptor d;
    d.dba = dba;
    d.dbc = dbc;
    d.dbc |= irq << 31;

    return d;
}

static void command_table_set_cfis(struct command_table* ct, struct sata_fis_reg_h2d* fis)
{
    assert(sizeof(ct->cfis) >= sizeof(*fis));
    struct sata_fis_reg_h2d* fis_ptr = (struct sata_fis_reg_h2d*) ct->cfis;
    *fis_ptr = *fis;
}

static errval_t port_init_fb(struct ahci_port* port)
{
    assert(port != NULL);

    errval_t err = dma_mem_alloc(1024, &port->fb);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "DMA alloc failed");
        return err_push(err, AHCI_ERR_PORT_INIT);
    }
    memset((void*)port->fb.vaddr, 0x0, 1024);

    assert(port->fb.paddr % 4096 == 0);
    ahci_port_fb_wr(&port->port, port->fb.paddr);
    return err;
}

static errval_t port_init_clb(struct ahci_port* port)
{
    assert (port != NULL);
    assert (ahci_port_chdr_size*32 == 0x400);

    errval_t err = dma_mem_alloc(ahci_port_chdr_size*32, &port->clb);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "DMA alloc failed");
        return err_push(err, AHCI_ERR_PORT_INIT);
    }
    memset((void*)port->clb.vaddr, 0x0, ahci_port_chdr_size*32);

    assert(port->clb.paddr % 1024 == 0);
    ahci_port_clb_wr(&port->port, port->clb.paddr);
    return err;
}

static errval_t port_init_ctba(struct ahci_port* port, size_t command_slot)
{
    assert(port != NULL);
    assert(command_slot < MAX_CTBA_SLOTS);

    errval_t err = dma_mem_alloc(4096, &port->ctba_mem[command_slot]);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "DMA alloc failed");
        return err_push(err, AHCI_ERR_PORT_INIT);
    }
    memset((void*)port->ctba_mem[command_slot].vaddr, 0x0, 4096);
    port->command_table[command_slot] = (struct command_table*) port->ctba_mem[command_slot].vaddr;

    return err;
}

static void blk_ahci_interrupt(struct ahci_port* port, struct dev_queue_request* reqs, 
                               size_t slots)
{
    ahci_port_is_t status = ahci_port_is_rd(&port->port);

    // A request was handled:
    //if (ahci_port_is_dhrs_extract(status) > 0) {

        for (size_t slot = 0; slot < slots; slot++) {
            struct dev_queue_request *dqr = &reqs[slot];

            bool slot_has_request = dqr->status == RequestStatus_InProgress;
            bool slot_done = ahci_port_slot_free(&port->port, slot);
            if (slot_has_request && !slot_done) {
                //printf("waiting for slot %zu.\n", slot);
            }
            if (slot_has_request && slot_done) {
                dqr->status = RequestStatus_Done;
            }
        }

        if (ahci_port_is_dhrs_extract(status)) {
            ahci_port_is_dhrs_wrf(&port->port, 0x1);
        }
    //}

    // Did something happen we can't yet handle?
    if (status > 0x1) {
#if defined(BLK_DEBUG_ENABLE)
        char buf[4096];
        ahci_port_is_pr(buf, 4096, &port->port);
        BLK_DEBUG("GOT unhandled IRQ: %u\n", ahci_port_is_rd(&port->port));
        puts(buf);
#else
        printf("[BLK] unhandled irq: %u", ahci_port_is_rd(&port->port));
#endif
    }
    else {
        //printf("[BLK] IRQ handled\n");
    }
}

errval_t blk_ahci_port_dma_async(struct ahci_port *port, size_t slot, uint64_t block, lpaddr_t base, size_t length, bool write)
{
    assert(length % 512 == 0);
    assert(ahci_port_slot_free(&port->port, 1 << slot));

    errval_t err = SYS_ERR_OK;

    ahci_port_chdr_t header = get_command_header(port, slot);
    memset(header, 0, ahci_port_chdr_size);
    ahci_port_chdr_a_insert(header, 0);
    ahci_port_chdr_w_insert(header, write);
    ahci_port_chdr_cfl_insert(header, sizeof(struct sata_fis_reg_h2d) / sizeof(uint32_t));
    ahci_port_chdr_prdtl_insert(header, 1);
    ahci_port_chdr_ctba_insert(header, port->ctba_mem[slot].paddr);

    uint8_t command = write ? ATA_CMD_WRITE_DMA_EXT : ATA_CMD_READ_DMA_EXT;
    uint16_t sectors = length / 512;

    struct command_table* ct = port->command_table[slot];
    struct sata_fis_reg_h2d fis;
    sata_h2d_fis_new(&fis, command, block, sectors);
    command_table_set_cfis(ct, &fis);
    ct->prdt[0] = region_descriptor_new(base, (length - 1) | 0x1, false);

    while (!ahci_port_is_ready(&port->port)) {
        
        // TODO: Abort return error on timeout
    }

    // Issue command in slot 0
    ahci_port_ci_rawwr(&port->port, 1 << slot);
    BLK_DEBUG("Issued async DMA command.\n");

#if 0
    while ((ahci_port_ci_rd(&port->port) & (1<<slot)) > 0) {
        // TODO: abort on timeout
        //barrelfish_usleep(10000);
    }
    BLK_DEBUG("Done with DMA command.\n");
#endif

    return err;
}

errval_t blk_ahci_port_dma(struct ahci_port *port, uint64_t block, struct dma_mem *buffer, bool write)
{
    errval_t err = SYS_ERR_OK;
    size_t slot = 0;

    // TODO: use only slot 0
    assert(ahci_port_slot_free(&port->port, 1 << slot));

    ahci_port_chdr_t header = get_command_header(port, slot);
    memset(header, 0, ahci_port_chdr_size);
    ahci_port_chdr_a_insert(header, 0);
    ahci_port_chdr_w_insert(header, write);
    ahci_port_chdr_cfl_insert(header, sizeof(struct sata_fis_reg_h2d) / sizeof(uint32_t));
    ahci_port_chdr_prdtl_insert(header, 1);
    ahci_port_chdr_ctba_insert(header, port->ctba_mem[slot].paddr);

    uint8_t command = write ? ATA_CMD_WRITE_DMA_EXT : ATA_CMD_READ_DMA_EXT;
    assert(buffer->bytes % 512 == 0);
    uint16_t sectors = buffer->bytes / 512;

    struct command_table* ct = port->command_table[slot];
    struct sata_fis_reg_h2d fis;
    sata_h2d_fis_new(&fis, command, block, sectors);
    command_table_set_cfis(ct, &fis);
    ct->prdt[0] = region_descriptor_new(buffer->paddr, (buffer->bytes - 1) | 0x1, false);

    while (!ahci_port_is_ready(&port->port)) {
        // TODO: Abort return error on timeout
    }

    // Issue command in slot 0
    ahci_port_ci_wr(&port->port, 1 << slot);

    BLK_DEBUG("Issued DMA command (w=%d) block=%zu.\n", write, block);
    while ((ahci_port_ci_rd(&port->port) & (1<<slot)) > 0) {
        // TODO: abort on timeout
        barrelfish_usleep(10000);
    }

    BLK_DEBUG("Done with DMA command.\n");
    // Clear expected interrupt:
    ahci_port_is_dhrs_wrf(&port->port, 0x1);

    if (ahci_port_is_rd(&port->port) != 0x0) {
#if defined(BLK_DEBUG_ENABLE)
        char buf[4096];
        ahci_port_is_pr(buf, 4096, &port->port);
        BLK_DEBUG("GOT unhandled IRQ: %u\n", ahci_port_is_rd(&port->port));
        puts(buf);
#endif
    }

    printf("%s:%s:%d: write? %d %zu\n", __FILE__, __FUNCTION__, __LINE__, write, ((uint64_t*)buffer->vaddr)[0]);
    return err;
}

static errval_t port_identify(struct ahci_port *port)
{
    // TODO: this function needs proper error handling
    errval_t err = SYS_ERR_OK;
    assert(ahci_port_slot_free(&port->port, 1));

    err = dma_mem_alloc(512, &port->identify_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "dma memory allocation failed.");
    }
    memset((void*)port->identify_mem.vaddr, 0, 512);

    // Write command into command list
    ahci_port_chdr_t header = get_command_header(port, 0);
    memset(header, 0, ahci_port_chdr_size);
    ahci_port_chdr_w_insert(header, 0);
    ahci_port_chdr_a_insert(header, 0);
    ahci_port_chdr_cfl_insert(header, sizeof(struct sata_fis_reg_h2d) / sizeof(uint32_t));
    ahci_port_chdr_prdtl_insert(header, 1); // we use 1 PRD
    ahci_port_chdr_ctba_insert(header, port->ctba_mem[0].paddr);

    struct command_table* ct = port->command_table[0];

    struct sata_fis_reg_h2d fis;
    memset(&fis, 0, sizeof(struct sata_fis_reg_h2d));
    fis.type = SATA_FIS_TYPE_H2D;
    fis.command = 0xEC; // ATA_CMD_IDENTIFY
    fis.device = 0;
    fis.specialstuff = 0x80; // Command
    command_table_set_cfis(ct, &fis);

    ct->prdt[0] = region_descriptor_new(port->identify_mem.paddr, 511, false);

    while (!ahci_port_is_ready(&port->port)) {}

    // Ensure command processing is on
    ahci_port_cmd_t cmd = ahci_port_cmd_rd(&port->port);
    assert(ahci_port_cmd_cr_extract(cmd) == 1);

    // Issue command in slot 0
    ahci_port_ci_wr(&port->port, 0x1);

    BLK_DEBUG("Issued IDENTIFY command.\n");
    while ((ahci_port_ci_rd(&port->port) & 0x1) > 0) {
        // TODO: abort on timeout
        barrelfish_usleep(10000);
    }

    // Clear the interrupts
    ahci_port_is_pss_wrf(&port->port, 0x1);

    // Nothing else should've happened:
    if (ahci_port_is_rd(&port->port) != 0x0) {
#if defined(BLK_DEBUG_ENABLE)
        char buf[4096];
        ahci_port_is_pr(buf, 4096, &port->port);
        BLK_DEBUG("GOT unhandled IRQ: %u\n", ahci_port_is_rd(&port->port));
        puts(buf);
#endif
    }
    assert(ahci_port_is_rd(&port->port) == 0x0);

    BLK_DEBUG("IDENTIFY command processed.\n");
    ata_identify_initialize(&port->identify, (mackerel_addr_t)port->identify_mem.vaddr);

#if defined(BLK_DEBUG_ENABLE)
    ahci_port_print_identification(&port->identify);
#endif
    return err;
}

errval_t blk_ahci_ports_init(struct ahci_disk *ad)
{
    errval_t err = SYS_ERR_OK;
    size_t ports = ahci_hba_get_num_ports(&ad->controller);
    size_t ncs = ahci_hba_cap_ncs_rdf(&ad->controller);
    BLK_DEBUG("Implemented ports: %zu\n", ports);
    BLK_DEBUG("Implemented command slots: %zu\n", ncs);

    lvaddr_t base_address = blk_ahci_get_bar5_vaddr(ad);

    // Determine which ports are implemented by the HBA, by reading the PI register. This bit
    // map value will aid software in determining how many ports are available and which port
    // registers need to be initialized.
    for (size_t i = 0; i < ports; i++) {
        struct ahci_port *port = &ad->ports[i];
        if (!ahci_port_is_implemented(&ad->controller, i)) {
            continue;
        }
        ahci_port_initialize(&port->port, (mackerel_addr_t)base_address + ahci_port_offset(i));

        BLK_DEBUG("Initializing port: %zu\n", i);
        uint32_t probe = ahci_port_probe(&port->port);
        if (probe == 0) {
            BLK_DEBUG("Port %zu no connection\n", i);
            continue;
        }
        else if (probe == 0x00000101) {
            BLK_DEBUG("Port %zu found SATA device, initializing.\n", i);
        }
        else {
            BLK_DEBUG("Port %zu unkown signature: %d\n", i, ahci_port_sig_rd(&port->port));
            continue;
        }

        // Ensure that the controller is not in the running state by reading and examining each
        // implemented port’s PxCMD register. If PxCMD.ST, PxCMD.CR, PxCMD.FRE and
        // PxCMD.FR are all cleared, the port is in an idle state.
        if (!ahci_port_is_idle(&port->port)) {
            BLK_DEBUG("Port %zu is running, stopping it.", i);
            ahci_port_stop(&port->port);
        }
        assert(ahci_port_is_idle(&port->port)); // if this triggers, try a port reset...

        // For each implemented port, system software shall allocate memory for and program:
        //  - PxCLB and PxCLBU
        //  - PxFB and PxFBU
        port_init_clb(port);
        port_init_fb(port);

        // After setting PxFB and PxFBU to the physical address of the FIS receive area,
        // system software shall set PxCMD.FRE to ‘1’.
        ahci_port_cmd_fre_wrf(&port->port, 0x1);

        // For each implemented port, clear the PxSERR register, by writing ‘1s’ to each bit location.
        uint32_t serr = ahci_port_serr_rd(&port->port);
        ahci_port_serr_wr(&port->port, serr);

        // Determine which events should cause an interrupt, and set each implemented port’s PxIE
        ahci_port_ie_wr(&port->port, 0xffffffff);

        assert(ncs < MAX_CTBA_SLOTS);
        for (size_t slot = 0; slot < ncs; slot++) {
            port_init_ctba(port, slot);
        }

        BLK_DEBUG("Waiting for port %zu to become ready\n", i);
        while(!ahci_port_is_functional(&port->port)) {
            barrelfish_usleep(5000);
        }
        ahci_port_start(&port->port);

        BLK_DEBUG("Initialized port %zu\n", i);
        err = port_identify(port);
        assert(err_is_ok(err));

        port->ncs = ahci_hba_get_command_slots(&ad->controller);
        port->interrupt = blk_ahci_interrupt;
        port->is_initialized = true;
    }

    return err;
}
