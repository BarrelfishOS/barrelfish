/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 * e1000_hwinit.c
 *
 *  Created on: Feb 12, 2013
 *      Author: mao
 *
 * Much referencing has been done against iPXE - Open Source Boot Firmware
 * and the Linux kernel.
 */
#include "e1000n.h"
#include "e1000n_hwinit.h"

#include <pci/pci_driver_client.h>
#include <driverkit/driverkit.h>


/*****************************************************************
 * PHY
 *
 ****************************************************************/
/*****************************************************************
 * Writes a value to a PHY register
 *
 *****************************************************************/
/* TODO */

/***************************************************************************
 * Is EEPROM NVM or FLASH.
 *
 * Returns true if EEPROM is of NVM type, else false.
 *
 ****************************************************************************/
#if 0
static bool e1000_is_onboard_nvm_eeprom(struct e1000_driver_state *eds)
{
    if (eds->mac_type == e1000_82573) {
        e1000_eecd_t eecd = e1000_eecd_rd(eds->device);
        /* Isolate bits 15 & 16 */
        uint8_t eecd_flash = ((eecd >> 15) & 0x03);

        /* If both bits are set, device is Flash type */
        if (eecd_flash == 0x03) {
            return false;
        }
    }

    return true;
}
#endif
/*****************************************************************
 * Read e1000 EEPROM.
 *
 * TODO: Fix semaphore support and eeprom release on devices that
 *       need this.
 *
 * dev     - device to read eeprom from.
 * offset  - eeprom offset.
 * data    - returns data read.
 * Returns:  0 on success, 1 on timeout and 2 if no eeprom.
 ****************************************************************/
#if 0
static errval_t e1000_read_eeprom(struct e1000_driver_state *eds, uint64_t offset,
                                  uint16_t *data)
{
    int timeout = 1000;

    /* Make sure there are no direct access requests on
     * devices that support this.
     */
    if (eds->mac_type == e1000_82574){
        e1000_eec_ee_req_wrf(eds->device, 1);
    } else if (eds->mac_type != e1000_82544) {
        e1000_eecd_ee_req_wrf(eds->device, 1);
    }

    if (eds->mac_type != e1000_I210) {
        while (!e1000_eecd_ee_gnt_rdf(eds->device)) {
            usec_delay(1000);
        }
    }

    /* EEPROM present */
    // TODO(gz): Why does e1000 82574 have ee_pres == 0? LH: QEMUs 82574 has ee_pres=1?
    if (e1000_eecd_ee_pres_rdf(eds->device) ||
            eds->mac_type == e1000_82574) {
        e1000_eerd_ms_t eerd_ms = 0;
        e1000_eerd_nm_t eerd_nm = 0;

        switch (eds->mac_type) {
        case e1000_82571:
        case e1000_82572:
        case e1000_82573:
        case e1000_82574:
        case e1000_82575:
		case e1000_I210:
		case e1000_I350:
            /* These devices have SPI or Microwire EEPROMs */
            eerd_ms = e1000_eerd_ms_start_insert(eerd_ms, 1);
            eerd_ms = e1000_eerd_ms_addr_insert(eerd_ms, offset);
            e1000_eerd_ms_wr(eds->device, eerd_ms);

            while (e1000_eerd_ms_done_rdf(eds->device) == 0 && 0 < timeout--) {
                usec_delay(1000);
            }

            *data = e1000_eerd_ms_data_rdf(eds->device);
            break;
        default:
            /* These devices have standard EEPROMs */
            eerd_nm = e1000_eerd_nm_start_insert(eerd_nm, 1);
            eerd_nm = e1000_eerd_nm_addr_insert(eerd_nm, offset);
            e1000_eerd_nm_wr(eds->device, eerd_nm);

            while (e1000_eerd_ms_done_rdf(eds->device) == 0 && 0 < timeout--) {
                usec_delay(1000);
            }

            *data = e1000_eerd_nm_data_rdf(eds->device);
            break;
        }
    } else {
        E1000_DEBUG("No EEPROM pressent.\n");
        e1000_eecd_ee_req_wrf(eds->device, 0);
        return -1;
    }

    if (timeout) {
        e1000_eecd_ee_req_wrf(eds->device, 0);
        return 0; /* Success */
    }

    E1000_DEBUG("EEPROM read timed out\n");

    e1000_eecd_ee_req_wrf(eds->device, 0);
    return 1;
}
#endif
/*****************************************************************
 * Check for EEPROM Auto Read bit done.
 *
 ****************************************************************/
#if 0
static errval_t e1000_get_auto_rd_done(struct e1000_driver_state *eds)
{
    uint16_t data;
    errval_t err;

    if(eds->mac_type == e1000_82574){
        // For the 82574  we just check the auto rd flag without issuing
        // an eeprom read. (FreeBSD does it like this)
        int timeout = 1000;
        while(timeout-- > 0){
            if(e1000_eec_82574_auto_rd_rdf(eds->device)){
                return SYS_ERR_OK;
            }
            usec_delay(10);
        }
        E1000_DEBUG("Timeout reached while waiting for auto_rd_done");
        return 1;
    } else {
        err = e1000_read_eeprom(eds, 0, &data);
    }

    /* PHY configuration from NVM just starts after EECD_AUTO_RD sets to high.
     * Need to wait for PHY configuration completion before accessing NVM
     * and PHY. */
    if (eds->mac_type == e1000_82573) {
        usec_delay(2500);
    }

    return err;
}
#endif
/******************************************************************************
 * Reads the adapter's MAC address from the EEPROM and inverts the LSB for the
 * second function of dual function devices
 *
 *****************************************************************************/
#if 0
static errval_t e1000_read_mac_addr(struct e1000_driver_state *eds, uint8_t *mac_addr)
{
    uint16_t offset;
    uint16_t eeprom_data, i;
    e1000_status_t status;

    for (i = 0; i < MAC_ADDRESS_LEN; i += 2) {
        offset = i >> 1;
        if (e1000_read_eeprom(eds, offset, &eeprom_data) != 0) {
            return 1;
        }
        mac_addr[i] = (uint8_t) (eeprom_data & 0x00FF);
        mac_addr[i + 1] = (uint8_t) (eeprom_data >> 8);
    }

    switch (eds->mac_type) {
    default:
        break;
    case e1000_82546:
    case e1000_82546_rev_3:
    case e1000_82571:
        /* test LAN ID to see if we need to modify the MAC from EEPROM */
        status = e1000_status_rd(eds->device);
        if (e1000_status_func_id_extract(status) == e1000_lan_b) {
            mac_addr[5] ^= e1000_lan_b_mask;
        }
        break;
    }

    return 0;
}
#endif
/*****************************************************************
 * Reset the device and disable interrupts.
 *
 ****************************************************************/
#if 0
static int e1000_reset(struct e1000_driver_state *eds)
{
    errval_t err = 0;
    int timeout;

    /* disable interrupts */
    if (eds->mac_type == e1000_I350) {
        e1000_eimc_wr(eds->device, 0xffffffff);
    } else {
        e1000_imc_rawwr(eds->device, 0xffffffff);
    }

    /* disable receive and transmit */
    e1000_rctl_rawwr(eds->device, 0);
    e1000_tctl_rawwr(eds->device, 0);

    /* Delay to allow outstanding PCI transactions to complete before
     * reseting the device */
    usec_delay(1000);

    /* Exit from GIO management mode */
    if (eds->mac_type == e1000_82571 || eds->mac_type == e1000_82563
            || eds->mac_type == e1000_82573) {
        E1000_DEBUG("Disabling GIO management.\n");

        e1000_ctrl_gio_md_wrf(eds->device, 1);

        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_ctrl_gio_md_rdf(eds->device) && 0 < timeout--);

        if (timeout <= 0) {
            E1000_DEBUG("Error: Failed to disable GIO management.\n");
            // return -1;
        }
    }

    if(eds->mac_type == e1000_82574){
        // 82574: Must poll on GIO Master Enable Status in status register
        E1000_DEBUG("Disabling GIO management.\n");
        e1000_ctrl_gio_md_wrf(eds->device, 1);

        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_status_gio_mes_rdf(eds->device) && 0 < timeout--);

        if (timeout <= 0) {
            E1000_DEBUG("Error: Failed to disable GIO management.\n");
            // return -1;
        }
    }

    if (eds->mac_type == e1000_I350) {
        E1000_DEBUG("Disabling GIO management.\n");
        e1000_ctrl_gio_md_wrf(eds->device, 1);
        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_status_I350_gio_mes_rdf(eds->device) && 0 < timeout--);

        if (timeout <= 0) {
            E1000_DEBUG("Error: Failed to disable GIO management.\n");
        }
    }

    /* Must reset PHY before reseting the MAC */
    if (eds->mac_type == e1000_82541 || eds->mac_type == e1000_82547) {
        e1000_ctrl_phy_rst_wrf(eds->device, 1);
    }

    /* Must acquire MDIO ownership before MAC reset
     * Ownership defaults to firmware after a reset */
    int mdio_acquired = false;
    if (eds->mac_type == e1000_82573 || eds->mac_type == e1000_82574) {
        timeout = 1000;
        do {
            e1000_extcnf_ctrl_mdio_swown_wrf(eds->device, 1);
            usec_delay(200);
        } while (e1000_extcnf_ctrl_mdio_swown_rdf(eds->device) == 0
                 && 0 < timeout--);
        if(timeout > 0){
            mdio_acquired = true;
        } else {
            E1000_DEBUG("Could not acquire MDIO software ownership.\n");
        }
    }

    E1000_DEBUG("Resetting device.\n");

    switch (eds->mac_type) {
    case e1000_82545_rev_3:
    case e1000_82546_rev_3:
        /* Reset is performed on a shadow of the control register
         * Where is this mentioned?
         */
        e1000_ctrldup_rst_wrf(eds->device, 1);
        break;
    case e1000_82540:
    case e1000_82541:
    case e1000_82541_rev_2:
    case e1000_82544:
    case e1000_82545:
    case e1000_82546:
        /* These controllers can't ack the 64-bit write when issuing the
         * reset, so use IO-mapping as a workaround to issue the reset
         * We don't support IO-mapped writing yet */
    case e1000_I350:
        e1000_ctrl_rst_wrf(eds->device, 1);
        usec_delay(3000);
        timeout = 1000;
        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_ctrl_rst_rdf(eds->device) != 0 && 0 < timeout--);

        if (timeout <= 0 || !e1000_status_pf_rst_done_rdf(eds->device)) {
            E1000_DEBUG("Error: Failed to reset device.\n");
        }
        break;

    case e1000_82574:
        e1000_ctrl_rst_wrf(eds->device, 1);
        usec_delay(10);
        break;

    default:
        e1000_ctrl_rst_wrf(eds->device, 1);

        /* Wait for reset to clear */
        timeout = 1000;
        do {
            usec_delay(10);
        } while (e1000_ctrl_rst_rdf(eds->device) != 0 && 0 < timeout--);

        if (timeout <= 0) {
            E1000_DEBUG("Error: Failed to reset device.\n");
        }
        break;
    }

    /*
     * If acquired, release mdio ownership
     */
    if (mdio_acquired) {
        timeout = 1000;
        do {
            e1000_extcnf_ctrl_mdio_swown_wrf(eds->device, 0);
            usec_delay(200);
        } while (e1000_extcnf_ctrl_mdio_swown_rdf(eds->device) == 1
                 && 0 < timeout--);
        if(timeout > 0){
            mdio_acquired = 0;
        } else {
            E1000_DEBUG("Could not release MDIO software ownership.\n");
        }
    }


    /* After MAC reset, force reload of EEPROM to restore power-on settings to
     * device.  Later controllers reload the EEPROM automatically, so just wait
     * for reload to complete.
     */
    switch (eds->mac_type) {
    case e1000_82542:
    case e1000_82543:
    case e1000_82544:
        e1000_ctrlext_ee_rst_wrf(eds->device, 1);
        /* Wait for EEPROM reload */
        usec_delay(2000);
        break;
    case e1000_82541:
    case e1000_82541_rev_2:
    case e1000_82547:
    case e1000_82547_rev_2:
        /* Wait for EEPROM reload */
        usec_delay(20000);
        break;
    case e1000_82573:
    case e1000_82574:
        if (e1000_is_onboard_nvm_eeprom(eds) == false) {
            usec_delay(100);
            e1000_ctrlext_ee_rst_wrf(eds->device, 1);
        }
        err = e1000_get_auto_rd_done(eds);
        break;
    case e1000_I350:
        timeout = 1000;
        while(!e1000_eec_auto_rd_rdf(eds->device) && timeout--) {
            usec_delay(100);
        }
        if (timeout <= 0) {
            E1000_DEBUG("Error: Autoloading of the EEPROM failed.\n");
        }
        usec_delay(3000);
        break;
    default:
        err = e1000_get_auto_rd_done(eds);

        break;
    }

    if (err) {
        E1000_DEBUG("Auto read by HW from EEPROM did not complete.\n");
    }

    /* Disable HW ARPs on ASF enabled adapters */
    if (eds->mac_type >= e1000_82540 && eds->mac_type <= e1000_82547_rev_2) {
        e1000_manc_arp_req_en_wrf(eds->device, 0);
    }

    if (eds->mac_type == e1000_82541 || eds->mac_type == e1000_82547) {
        /* Configure activity LED after PHY reset */
        e1000_ledctl_t ledctl;

        // TODO:
//      e1000_phy_init_script(dev);

        /* I guess this is not realy needed to setup card LEDs */
        ledctl = e1000_ledctl_rd(eds->device);
        ledctl &= IGP_ACTIVITY_LED_MASK;
        ledctl = e1000_ledctl_led0_mode_insert(ledctl, 0x2);
        ledctl = e1000_ledctl_led3_mode_insert(ledctl, 0x3);
        e1000_ledctl_wr(eds->device, ledctl);
    }

    /* disable interrupts */
    if (eds->mac_type == e1000_I350) {
        e1000_eimc_wr(eds->device, 0xffffffff);
    } else {
        e1000_imc_rawwr(eds->device, 0xffffffff);
    }

    /* clear any pending interrupts */
    e1000_icr_rd(eds->device);

    debug_printf("Reset done..\n");

    return 0;
}
#endif

static int e1000_reset(struct e1000_driver_state *device)
{
    // errval_t err = 0;
    int timeout;
    e1000_t *hw_device = device->device;

    /* disable interrupts */
    if (device->extended_interrupts) {
        e1000_eimc_wr(hw_device, 0xffffffff);
    }
    e1000_imc_rawwr(hw_device, 0xffffffff);

    /* disable receive and transmit */
    e1000_rctl_rawwr(hw_device, 0);
    e1000_tctl_rawwr(hw_device, 0);

    /* Delay to allow outstanding PCI transactions to complete before
     * reseting the device */
    // usec_delay(1000);
    e1000_ctrl_phy_rst_wrf(hw_device, 1);
    e1000_ctrl_rst_wrf(hw_device, 1);
    /* Wait for reset to clear */
    timeout = 1000;

    do {
        usec_delay(10);
    } while (e1000_ctrl_rst_rdf(hw_device) != 0 && 0 < timeout--);
    assert(timeout >= 0);
    E1000_DEBUG("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);

    /* clear any pending interrupts */
    e1000_icr_rd(hw_device);
    if (device->extended_interrupts) {
        e1000_eicr_rd(hw_device);
    }
    
    e1000_ctrl_phy_rst_wrf(hw_device, 0);

    debug_printf("Reset done..\n");

    return 0;
}

/*****************************************************************
 * Get media type.
 *
 ****************************************************************/
static void e1000_set_media_type(struct e1000_driver_state *eds)
{
    e1000_status_t status;

    if (eds->mac_type != e1000_82543) {
        eds->tbi_combaility = false;
    }

    switch (eds->pdc.id.device) {
    case E1000_DEVICE_82545GM_SERDES:
    case E1000_DEVICE_82546GB_SERDES:
    case E1000_DEVICE_82571EB_SERDES:
    case E1000_DEVICE_82571EB_SERDES_DUAL:
    case E1000_DEVICE_82571EB_SERDES_QUAD:
    case E1000_DEVICE_82572EI_SERDES:
        eds->media_type = e1000_media_type_serdes;
        break;
    default:
        switch (eds->mac_type) {
            /*
             * According to: 1.3.7 Additional Ethernet Controller Features
             */
        case e1000_82546:
        case e1000_82545:
            eds->media_type = e1000_media_type_serdes;
            break;
        case e1000_82542:
            eds->media_type = e1000_media_type_fiber;
            break;
        case e1000_82573:
            /* The STATUS.tbimode bit is reserved or reused for the this
             * device.
             */
            eds->media_type = e1000_media_type_copper;
            break;
        default:
            status = e1000_status_rd(eds->device);
            if (e1000_status_tbimode_extract(status)) {
                eds->media_type = e1000_media_type_fiber;
                eds->tbi_combaility = false;
            } else {
                eds->media_type = e1000_media_type_copper;
            }
            break;
        }
        break;
    }
}

/*****************************************************************
 * Check link connection status.
 *
 ****************************************************************/
bool e1000_check_link_up(e1000_t *device)
{
    e1000_status_t status = e1000_status_rd(device);

    if (e1000_status_lu_extract(status)) {
        return true;
    }

    return false;
}

/*****************************************************************
 * Setup link auto-negotiation.
 *
 ****************************************************************/
bool e1000_auto_negotiate_link(e1000_t *device, e1000_mac_type_t mac)
{
    bool link_up = false;

    e1000_ctrlext_t ctrlext = e1000_ctrlext_rd(device);
    if (e1000_ctrlext_link_mode_extract(ctrlext) == e1000_serdes) {
        E1000_DEBUG("Auto-negotiation: serdes mode");
        int timeout = 4000;
        e1000_txcw_ane_wrf(device, 1);
        e1000_ctrl_lrst_wrf(device, 1);

        while (e1000_rxcw_anc_rdf(device) == 0 && 0 < timeout--) {
            usec_delay(10);
        }

        if (timeout > 0) {
            link_up = true;
        }

        if (!link_up) {
            e1000_txcw_ane_wrf(device, 0);
        }
    } else {
        int timeout = 4000;

        // XXX: find out which cards really need this?
        if (mac < e1000_82571) {
            e1000_ctrl_asde_wrf(device, 1);
        }

        if (mac == e1000_I350) {
            e1000_ctrl_slu_wrf(device, 1);
            e1000_ctrl_frcspd_wrf(device, 0);
            e1000_ctrl_frcdplx_wrf(device, 0);
        }

        while (e1000_check_link_up(device) == false && 0 < timeout--) {
            usec_delay(10);
        }

        link_up = e1000_check_link_up(device);
    }

    E1000_DEBUG("Auto-negotiate link status: %s\n", e1000_check_link_up(device) ? "link-up" : "link-down");
    return link_up;
}

/*****************************************************************
 * Set RX buffer size and enable receive unit.
 *
 ****************************************************************/
static void e1000_set_rxbsize(struct e1000_driver_state *eds, e1000_rx_bsize_t rx_bsize)
{
    uint8_t bsize;
    uint8_t bsex;
    e1000_rctl_t rctl;

    switch (rx_bsize) {
    case bsize_16384:
        bsize = 0x1;
        bsex = 1;
        break;
    case bsize_8192:
        bsize = 0x2;
        bsex = 1;
        break;
    case bsize_4096:
        bsize = 0x3;
        bsex = 1;
        break;
    case bsize_2048:
        bsize = 0x0;
        bsex = 0;
        break;
    case bsize_1024:
        bsize = 0x1;
        bsex = 0;
        break;
    case bsize_512:
        bsize = 0x2;
        bsex = 0;
        break;
    case bsize_256:
    default:
        bsize = 0x3;
        bsex = 0;
        break;
    }

    rctl = e1000_rctl_rd(eds->device);
    rctl = e1000_rctl_bsize_insert(rctl, bsize);
    rctl = e1000_rctl_bsex_insert(rctl, bsex);
    rctl = e1000_rctl_bam_insert(rctl, 1);
    e1000_rctl_wr(eds->device, rctl);

    e1000_rctl_en_wrf(eds->device, 1);
}

/*****************************************************************
 * Set serial interface mode.
 *
 ****************************************************************/
static void e1000_set_serial_interface_mode(struct e1000_driver_state *eds)
{
    e1000_ctrlext_t ctrlext = e1000_ctrlext_rd(eds->device);

    if (eds->mac_type == e1000_82544) {
        assert(!"XXX: How do we set these ones up?");
        return;
    }

    if (eds->mac_type == e1000_82573) {
        ctrlext = e1000_ctrlext_link_mode_insert(ctrlext, e1000_l82573);
    }
    else if (eds->media_type == e1000_media_type_serdes) {
        ctrlext = e1000_ctrlext_link_mode_insert(ctrlext, e1000_serdes);
    }
    else {
        ctrlext = e1000_ctrlext_link_mode_insert(ctrlext, e1000_glci);
    }
    /* write serial interface mode */
    e1000_ctrlext_wr(eds->device, ctrlext);
}

/*****************************************************************
 * Set Transmit Inter Packet Gap (TIPG)
 *
 ****************************************************************/
static void e1000_set_tipg(struct e1000_driver_state *eds)
{
    e1000_tipg_t tipg = 0;

    if ((eds->mac_type <= e1000_82547_rev_2)
            && (eds->media_type == e1000_media_type_fiber
                || eds->media_type == e1000_media_type_serdes)) {
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT_FIBER);
    } else {
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT_COPPER);
    }

    switch (eds->mac_type) {
    case e1000_82542:
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT);
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82542_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82542_TIPG_IPGR2);
        break;
    case e1000_82575:
    case e1000_82576:
    case e1000_I210:
    case e1000_I219:
    case e1000_I350:
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82575_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82575_TIPG_IPGR2);
        break;
    default:
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82543_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82543_TIPG_IPGR2);
        break;
    }

    e1000_tipg_wr(eds->device, tipg);
}

/*****************************************************************
 * Configure device receive
 *
 ****************************************************************/
static void e1000_setup_rx(struct e1000_driver_state *device, struct capref rx)
{
    e1000_t *hw_device = device->device;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    errval_t err;

    err = invoke_frame_identify(rx, &frameid);
    assert(err_is_ok(err));

    /* clear MTA table */
    for (int i = 0; i < e1000_mta_length; i++) {
        e1000_mta_wr(hw_device, i, 0);
    }

    switch (device->mac_type) {
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /*  Software should program RDLEN[n] register only when queue is disabled */
            e1000_rdbal_I350_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_rdbah_I350_wr(hw_device, 0, (frameid.base >> 32) & 0xffffffff);
            e1000_rdlen_I350_len_wrf(hw_device, 0, (device->receive_buffers / 8));

            /* Initialize receive head and tail pointers */
            e1000_rdh_I350_wr(hw_device, 0, 0);
            e1000_rdt_I350_wr(hw_device, 0, 0);
        } break;
        default: {
            /* tell card where receive ring is */
            e1000_rdbal_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_rdbah_wr(hw_device, 0, (frameid.base >> 32) & 0xffffffff);
            e1000_rdlen_len_wrf(hw_device, 0, (device->receive_buffers / 8));

            /* Initialize receive head and tail pointers */
            e1000_rdh_wr(hw_device, 0, 0);
            e1000_rdt_wr(hw_device, 0, 0);
        } break;
    }

    /* set buffer size and enable receive unit */
    e1000_set_rxbsize(device, bsize_2048);

    /* receive descriptor control */
    switch (device->mac_type) {
        case e1000_82575:
        {
            e1000_rxdctl_82575_t rxdctl = 0;

            rxdctl = e1000_rxdctl_82575_enable_insert(rxdctl, 1);
            rxdctl = e1000_rxdctl_82575_wthresh_insert(rxdctl, 1);
            e1000_rxdctl_82575_wr(hw_device, 0, rxdctl);
            
            E1000_DEBUG("%s: rxdctl %x\n", __func__, e1000_rxdctl_82575_rd(hw_device, 0));
        } break;
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /* If VLANs are not used, software should clear VFE. */
            e1000_rctl_vfe_wrf(hw_device, 0);

             /* Set up the MTA (Multicast Table Array) by software. This means
              * zeroing all entries initially and adding in entries as requested. */
            for (int i = 0; i < 128; ++i) {
                e1000_mta_wr(hw_device, i, 0);
            }

            /* Program SRRCTL of the queue according to the size of the buffers,
             * the required header handling and the drop policy. */
            e1000_srrctl_t srrctl = 0;
            srrctl = e1000_srrctl_bsizeheader_insert(srrctl, 0);
            e1000_srrctl_wr(hw_device, 0, srrctl);

            /* Enable the queue by setting RXDCTL.ENABLE. In the case of queue zero,
             * the enable bit is set by default - so the ring parameters should be
             * set before RCTL.RXEN is set. */
            e1000_rxdctl_I350_t rxdctl = 0;
            rxdctl = e1000_rxdctl_I350_enable_insert(rxdctl, 1);
            rxdctl = e1000_rxdctl_I350_wthresh_insert(rxdctl, 1);
            e1000_rxdctl_I350_wr(hw_device, 0, rxdctl);

            /* Poll the RXDCTL register until the ENABLE bit is set. The tail should
             * not be bumped before this bit was read as one. */
            int timeout = 1000;
            while (!e1000_rxdctl_I350_enable_rdf(hw_device, 0) && timeout--) {
                // usec_delay(10);
            }
            E1000_DEBUG("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);
            // if (timeout <= 0) {
            //     E1000_DEBUG("ERROR: failed to enable the RX queue\n");
            // }
        } break;
        default: {
            e1000_rxdctl_t rxdctl = 0;

            rxdctl = e1000_rxdctl_gran_insert(rxdctl, 1);
            rxdctl = e1000_rxdctl_wthresh_insert(rxdctl, 1);
            e1000_rxdctl_wr(hw_device, 0, rxdctl);

            e1000_rfctl_exsten_wrf(hw_device, 0);
        } break;
    }
    
    debug_printf("%s: rctl:%x  rxdctl:%x\n", __func__, e1000_rctl_rd(hw_device), e1000_rxdctl_rd(hw_device, 0));
}

/*****************************************************************
 * Configure card transmit
 *
 ****************************************************************/

static void e1000_setup_tx(struct e1000_driver_state *device, struct capref tx)
{
    e1000_t *hw_device = device->device;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    errval_t err;

    err = invoke_frame_identify(tx, &frameid);
    assert(err_is_ok(err));

    switch (device->mac_type) {
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /* Software should program TDLEN[n] register only when queue is disabled */
            e1000_tdbal_I350_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_tdbah_I350_wr(hw_device, 0, frameid.base >> 32);
            e1000_tdlen_I350_len_wrf(hw_device, 0, (device->transmit_buffers / 8));
            e1000_tdh_I350_wr(hw_device, 0, 0);
            e1000_tdt_I350_wr(hw_device, 0, 0);
        } break;
        default: {
            /* tell card about our transmit ring */
            e1000_tdbal_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_tdbah_wr(hw_device, 0, frameid.base >> 32);
            e1000_tdlen_len_wrf(hw_device, 0, (device->transmit_buffers/ 8));
            e1000_tdh_wr(hw_device, 0, 0);
            e1000_tdt_wr(hw_device, 0, 0);
        } break;
    }
    
    /* --------------------- transmit setup --------------------- */
    switch (device->mac_type) {
        case e1000_82575:
        {
            e1000_txdctl_82575_t txdctl = 0;
            txdctl = e1000_txdctl_82575_enable_insert(txdctl, 1);
            txdctl = e1000_txdctl_82575_priority_insert(txdctl, 1);
            e1000_txdctl_82575_wr(hw_device, 0, txdctl);
        } break;
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /* Program the TXDCTL register with the desired TX descriptor write
             * back policy. Suggested values are:
                    — WTHRESH = 1b
                    — All other fields 0b.
             */
            e1000_txdctl_I350_t txdctl = 0;
            // txdctl = e1000_txdctl_I350_priority_insert(txdctl, 1);
            txdctl = e1000_txdctl_I350_wthresh_insert(txdctl, 1);
            e1000_txdctl_I350_wr(hw_device, 0, txdctl);

            /* If needed, set the TDWBAL/TWDBAH to enable head write back */
            e1000_tdwbal_wr(hw_device, 0, 0);
            e1000_tdwbah_wr(hw_device, 0, 0);

            /* Enable the queue using TXDCTL.ENABLE (queue zero is enabled by default). */
            e1000_txdctl_I350_enable_wrf(hw_device, 0, 1);
        } break;
        default: {
            e1000_txdctl_t txdctl = 0;
            txdctl = e1000_txdctl_gran_insert(txdctl, 1);
            txdctl = e1000_txdctl_wthresh_insert(txdctl, 1);
            e1000_txdctl_wr(hw_device, 0, txdctl);
        } break;
    }
    /* enable transmit */

    e1000_tctl_t tctl = 0;
    tctl = e1000_tctl_ct_insert(tctl, 0xf);
    tctl = e1000_tctl_en_insert(tctl, 1);
    tctl = e1000_tctl_psp_insert(tctl, 1);
    e1000_tctl_wr(hw_device, tctl);

    if (device->mac_type == e1000_82576 || device->mac_type == e1000_I210 || 
        device->mac_type == e1000_I350  || device->mac_type == e1000_I219 ) {
            /* Poll the TXDCTL register until the ENABLE bit is set. */
            int timeout = 1000;
            while(!e1000_txdctl_I350_enable_rdf(hw_device, 0) && timeout--) {
                // usec_delay(10);
            }
            debug_printf("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);
            // if (timeout <= 0) {
            //     E1000_DEBUG("ERROR: failed to enable the TX queue\n");
            // }
    }
    e1000_set_tipg(device);
    debug_printf("%s: tctl:%x  txdctl:%x\n", __func__, e1000_tctl_rd(hw_device), e1000_txdctl_rd(hw_device, 0));
}
/*
 * Set interrupt throttle for all interrupts
 */
void e1000_set_interrupt_throttle(struct e1000_driver_state *eds, uint16_t usec)
{
    /* Enable interrupt throttling rate.
     *
     * The optimal performance setting for this register is very system and
     * configuration specific. A initial suggested range is 651-5580 (28Bh - 15CCh).
     * The value 0 will disable interrupt throttling
     */
    int16_t rate = usec * 4;
    
    if (eds->mac_type == e1000_82575
        || eds->mac_type == e1000_82576
        || eds->mac_type == e1000_I210
        || eds->mac_type == e1000_I219
        || eds->mac_type == e1000_I350) {
        // TODO(lh): Check if these cards really dont need the itr set as well.
        e1000_eitr_interval_wrf(eds->device, 0, rate);
        e1000_eitr_interval_wrf(eds->device, 1, rate);
        e1000_eitr_interval_wrf(eds->device, 2, rate);
        e1000_eitr_interval_wrf(eds->device, 3, rate);
    }
    else if(eds->mac_type == e1000_82574){
        e1000_itr_interval_wrf(eds->device, rate);
        e1000_eitr_82574_interval_wrf(eds->device, 0, rate);
        e1000_eitr_82574_interval_wrf(eds->device, 1, rate);
        e1000_eitr_82574_interval_wrf(eds->device, 2, rate);
        e1000_eitr_82574_interval_wrf(eds->device, 3, rate);
    }
    else {
        e1000_itr_interval_wrf(eds->device, rate);
    }
}


void e1000_init_queues(struct e1000_driver_state* eds, struct capref rx, 
                       size_t rx_bufs, struct capref tx, size_t tx_bufs)
{
    eds->transmit_buffers = tx_bufs;
    eds->receive_buffers = rx_bufs;
    
    e1000_setup_tx(eds, tx);
    
    e1000_setup_rx(eds, rx);
}

/*
 * setup MAC
 */
static void e1000_setup_mac(struct e1000_driver_state *device, uint64_t *mac_addr)
{
    e1000_t *hw_device = device->device;
    
    /* is a valid MAC already present? */
    /* This will always return false due to hardware/software reset */
    bool mac_present = e1000_rah_av_rdf(hw_device, 0);
    
    assert(mac_present);
    /* cache MAC for stack to see */
    uint64_t mac_hi = e1000_rah_rah_rdf(hw_device, 0);
    uint64_t mac = e1000_ral_rd(hw_device, 0) + (mac_hi << 32);
    *mac_addr = mac | (mac_hi << 32);

    debug_printf("%s: MAC address: %lx\n", device->service_name, *mac_addr);
    memcpy(device->mac_address, mac_addr, MAC_ADDRESS_LEN);

    /* clear all other filers (clear high-to-low (13.4.3)) */
    for (int i = 1; i < e1000_ral_length; i++) {
        e1000_rah_wr(hw_device, i, 0);
        e1000_ral_wr(hw_device, i, 0);
    }

}

/*****************************************************************
 * Initialize the hardware
 *
 ****************************************************************/
void e1000_hwinit(struct e1000_driver_state *eds)
{
    errval_t err;
    int num_bars = pcid_get_bar_num(&eds->pdc);

    E1000_DEBUG("Initializing network device.\n");

    if (num_bars < 1) {
        E1000_PRINT_ERROR("Error: Not enough PCI bars allocated. Can not initialize network device.\n");
        exit(1);
    }

    lvaddr_t vaddr;

    err = pcid_get_bar_cap(&eds->pdc, 0, &eds->regs);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pcid_get_bar_cap");
        E1000_PRINT_ERROR("Error: pcid_get_bar_cap. Will not initialize"
                " MSIx controller.\n");
        exit(1);
    }

    err = map_device_cap(eds->regs, &vaddr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pcid_map_bar");
        E1000_PRINT_ERROR("Error: map_device failed. Will not initialize"
                " MSIx controller.\n");
        exit(1);
    }

    e1000_initialize(&eds->device_inst, (void *) vaddr);
    eds->device = &eds->device_inst;

	/*
	 * XXX: This is a check if we are using legacy descriptors and virtual functions
	 *      are enabled to display an error message and abort execution.
	 */
    if (eds->mac_type == e1000_I350 && E1000_USE_LEGACY_DESC) {
        if(e1000_txswc_loopback_en_rdf(eds->device)
               || e1000_status_I350_vfe_rdf(eds->device)
               || e1000_txswc_macas_rdf(eds->device)
               || e1000_txswc_vlanas_rdf(eds->device)) {
            debug_printf("ERROR: legacy descriptors used with Advanced Features!\n");
            exit(1);
        };

    }

    E1000_DEBUG("Setting media type.\n");
    e1000_set_media_type(eds);

    err = e1000_reset(eds);
    if (err) {
        E1000_DEBUG("Error during e1000_reset! Exiting.");
        exit(1);
    }

    E1000_DEBUG("Deasserting PHY reset.\n");
    e1000_ctrl_phy_rst_wrf(eds->device, 0);

    E1000_DEBUG("Setting serial interface mode.\n");
    e1000_set_serial_interface_mode(eds);

    E1000_DEBUG("Auto negotiating link.\n");
    if (!e1000_auto_negotiate_link(eds->device, eds->mac_type)) {
        E1000_DEBUG("Auto negotiating link failed, force link-up in driver.\n");
        e1000_ctrl_slu_wrf(eds->device, 0x1);
        //set full-duplex
        e1000_ctrl_fd_wrf(eds->device, 0x1);
        e1000_ctrl_speed_wrf(eds->device, e1000_status_speed_rdf(eds->device));
    }

    if (eds->mac_type == e1000_I350) {
        e1000_ctrl_rfce_wrf(eds->device, 0);
        e1000_ctrl_tfce_wrf(eds->device, 0);
    }
    /* set flow control */
    e1000_fcal_wr(eds->device, 0);
    e1000_fcah_wr(eds->device, 0);
    e1000_fct_wr(eds->device, 0);

    /* initialize statistic counters */
    for (int i = 0; i < e1000_statsregs_length; i++) {
        e1000_statsregs_rd(eds->device, i);
    }

    /* --------------------- MAC address setup --------------------- */
    E1000_DEBUG("Setting up MAC address.\n");

    uint64_t mac;
    e1000_setup_mac(eds, &mac);

    /* clear MTA table */
    for (int i = 0; i < e1000_mta_length; i++) {
        e1000_mta_wr(eds->device, i, 0);
    }

    /* --------------------- receive setup --------------------- */
    /* receive descriptor control */
    if (eds->mac_type == e1000_82575
        || eds->mac_type == e1000_82576
        || eds->mac_type == e1000_I210
        || eds->mac_type == e1000_I219) {
        e1000_rxdctl_82575_t rxdctl = 0;

        rxdctl = e1000_rxdctl_82575_enable_insert(rxdctl, 1);
        rxdctl = e1000_rxdctl_82575_wthresh_insert(rxdctl, 1);
        e1000_rxdctl_82575_wr(eds->device, 0, rxdctl);
    }  else if (eds->mac_type != e1000_I350) {
        e1000_rxdctl_t rxdctl = 0;

        rxdctl = e1000_rxdctl_gran_insert(rxdctl, 1);
        rxdctl = e1000_rxdctl_wthresh_insert(rxdctl, 1);
        e1000_rxdctl_wr(eds->device, 0, rxdctl);

        e1000_rfctl_exsten_wrf(eds->device, 0);
    }

    /* Enable interrupts */
    if (eds->use_interrupt) {
        e1000_set_interrupt_throttle(eds, E1000_DEFAULT_INT_THROTTLE_RATE);

        e1000_intreg_t intreg = 0;
        /* Activate link change interrupt */
        intreg = e1000_intreg_lsc_insert(intreg, 1);
        /* Activate rx0 interrupt */
        intreg = e1000_intreg_rxt0_insert(intreg, 1);
        if(eds->mac_type == e1000_82574){
            /* Activate other MSIx causes, unfortunately, mackerel has a
             * conflicting definition so set the bitmask manually for now */
            #define ICR_E1000E_OTHER 24
            intreg |= 1 << ICR_E1000E_OTHER;
        }
        e1000_ims_rawwr(eds->device, intreg);

        /* In case of the 82574, we explicitly activate int cause auto clear to
         * get the same behaviour as the other cards */
        if(eds->mac_type == e1000_82574){
            e1000_ctrlext_iame_wrf(eds->device, 1);
        }
    }

#ifdef UNDER_TEST
    eds->msix = e1000_supports_msix(eds->mac_type);
#else 
    eds->msix = false;
#endif
}
