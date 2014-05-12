/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>

#ifdef OMAP_SDMA_KERNELBENCH
    #include <arch/armv7/gic.h>
    #include <arm_hal.h>
    #include <exceptions.h>
    #include <kernel.h>
#else
    #include <barrelfish/waitset.h>
    #include <barrelfish/inthandler.h>
    #include <driverkit/driverkit.h>
#endif

#include <string.h>

#include "omap_sdma.h"
#include <omap44xx_map.h>

static omap44xx_sdma_t devsdma;
static bool allocated_channel[OMAP44XX_SDMA_NUM_CHANNEL];

static omap_sdma_irq_handler_t irq_callback;

static inline errval_t omap_sdma_read_csr(omap44xx_sdma_dma4_csr_t csr)
{
    if (omap44xx_sdma_dma4_csr_misaligned_adrs_err_extract(csr)) {
        return OMAP_SDMA_ERR_MISALIGNED_ADDRESS;
    } else if (omap44xx_sdma_dma4_csr_supervisor_err_extract(csr)) {
        return OMAP_SDMA_ERR_SUPERVISOR;
    } else if (omap44xx_sdma_dma4_csr_trans_err_extract(csr)) {
        return OMAP_SDMA_ERR_TRANSACTION;
    }

    return SYS_ERR_OK;
}

static void omap_sdma_irq_handler(void *arg)
{
    uint32_t irqstatus = omap44xx_sdma_dma4_irqstatus_line_rd(&devsdma, OMAP44XX_SDMA_IRQ_LINE);

    for (omap_sdma_channel_t channel=0; channel<OMAP44XX_SDMA_NUM_CHANNEL; channel++) {
        bool active = (irqstatus >> channel) & 0x1;
        if(!active) continue;

        SDMA_PRINT("interrupt on channel %u\n", channel);

        // read out status flags
        omap44xx_sdma_dma4_csr_t csr = omap44xx_sdma_dma4_csr_rd(&devsdma, channel);

        // check for errors
        errval_t err = omap_sdma_read_csr(csr);

        if (err_is_ok(err)) {
            // no error found, check for "End of Block" event
            if(omap44xx_sdma_dma4_csr_block_extract(csr)) {
                irq_callback(channel, err);
            }
        } else {
            // OMAP4460 Multimedia Device Silicon Errata, Revision A:
            // 1.7 sDMA Channel Is Not Disabled After A Transaction Error
            if (err_no(err) == OMAP_SDMA_ERR_TRANSACTION) {
                // Workaround: disable channel by software
                omap44xx_sdma_dma4_ccr_enable_wrf(&devsdma, channel, 0);
            }

            irq_callback(channel, err);
        }

        // clear all read status flags
        omap44xx_sdma_dma4_csr_wr(&devsdma, channel, csr);
    }

    SDMA_PRINT("interrupt finished\n");

    // clear all set status bits
    omap44xx_sdma_dma4_irqstatus_line_wr(&devsdma, OMAP44XX_SDMA_IRQ_LINE, irqstatus);
}


#ifdef OMAP_SDMA_KERNELBENCH
// This interrupt handler is for use in the SDMA kernel benchmark only!
// It depends on GCC's code generation to create the prologue and epilogue
// for it to be a valid interrupt handler.
//
// Vanilla Barrelfish is not designed handle interrupts in the kernel.
__attribute__((interrupt("IRQ")))
static void omap_sdma_kernel_irq_handler(void)
{
    int irq = gic_get_active_irq();
    if(irq == OMAP44XX_SDMA_IRQ) {
        omap_sdma_irq_handler(NULL);
    }
    gic_ack_irq(irq);
}
#endif

static void omap_sdma_irq_config(omap_sdma_channel_t channel)
{
    omap44xx_sdma_dma4_cicr_t dma4_cicr = omap44xx_sdma_dma4_cicr_rd(&devsdma, channel);

    dma4_cicr = omap44xx_sdma_dma4_cicr_super_block_ie_insert(dma4_cicr, 0x0);
    dma4_cicr = omap44xx_sdma_dma4_cicr_drain_ie_insert(dma4_cicr, 0x0);
    dma4_cicr = omap44xx_sdma_dma4_cicr_misaligned_err_ie_insert(dma4_cicr, 0x1);
    dma4_cicr = omap44xx_sdma_dma4_cicr_supervisor_err_ie_insert(dma4_cicr, 0x1);
    dma4_cicr = omap44xx_sdma_dma4_cicr_trans_err_ie_insert(dma4_cicr, 0x1);
    dma4_cicr = omap44xx_sdma_dma4_cicr_pkt_ie_insert(dma4_cicr, 0x0);
    dma4_cicr = omap44xx_sdma_dma4_cicr_block_ie_insert(dma4_cicr, 0x1);
    dma4_cicr = omap44xx_sdma_dma4_cicr_last_ie_insert(dma4_cicr, 0x0);
    dma4_cicr = omap44xx_sdma_dma4_cicr_frame_ie_insert(dma4_cicr, 0x0);
    dma4_cicr = omap44xx_sdma_dma4_cicr_half_ie_insert(dma4_cicr, 0x0);
    dma4_cicr = omap44xx_sdma_dma4_cicr_drop_ie_insert(dma4_cicr, 0x0);

    omap44xx_sdma_dma4_cicr_wr(&devsdma, channel, dma4_cicr);
}

/**
 * \brief Initialzes a channel configuraton struct with its reset values.
 */
void omap_sdma_init_channel_conf(struct omap_sdma_channel_conf *conf) {
    // this function initializes the config struct with default values

    // TRM reset values
    conf->read_priority  = omap44xx_sdma_PORT_PRIORITY_LOW;
    conf->write_priority = omap44xx_sdma_PORT_PRIORITY_LOW;

    conf->color_mode = omap44xx_sdma_DISABLE_COLOR_MODE;
    conf->color = 0x000000;

    // no reset value here, use sane default
    conf->write_mode = omap44xx_sdma_WRITE_MODE_LAST_NON_POSTED;

    struct omap_sdma_transfer_size *transfer_size = &conf->transfer_size;
    transfer_size->element_number = 0;
    transfer_size->frame_number = 0;
    transfer_size->data_type = omap44xx_sdma_DATA_TYPE_32BIT;

    // default transfer config
    struct omap_sdma_transfer_conf *src_conf = &conf->src_conf;
    struct omap_sdma_transfer_conf *dst_conf = &conf->dst_conf;
    src_conf->start_address = 0;
    src_conf->addr_mode = omap44xx_sdma_ADDR_MODE_POST_INCR;
    src_conf->element_index = 0;
    src_conf->frame_index = 0;
    src_conf->packed_transfer = omap44xx_sdma_SRC_PACKED_DISABLE;
    src_conf->burst_mode = omap44xx_sdma_BURST_EN_SINGLE;

    // use the same default values for the destination port
    memcpy(dst_conf, src_conf, sizeof(struct omap_sdma_transfer_conf));

    conf->enable_link = false;
    conf->next_channel = 0;
}

static omap44xx_sdma_dma4_ccr_t omap_sdma_channel_conf_ccr(
                                omap44xx_sdma_dma4_ccr_t dma4_ccr,
                                struct omap_sdma_channel_conf *conf)
{
    dma4_ccr = omap44xx_sdma_dma4_ccr_src_amode_insert(dma4_ccr, conf->src_conf.addr_mode);
    dma4_ccr = omap44xx_sdma_dma4_ccr_dst_amode_insert(dma4_ccr, conf->dst_conf.addr_mode);

    assert(
        (conf->read_priority == omap44xx_sdma_PORT_PRIORITY_LOW) ||
        (conf->read_priority == omap44xx_sdma_PORT_PRIORITY_HIGH)
    );
    dma4_ccr = omap44xx_sdma_dma4_ccr_read_priority_insert(dma4_ccr, conf->read_priority);

    assert(
        (conf->write_priority == omap44xx_sdma_PORT_PRIORITY_LOW) ||
        (conf->write_priority == omap44xx_sdma_PORT_PRIORITY_HIGH)
    );
    dma4_ccr = omap44xx_sdma_dma4_ccr_write_priority_insert(dma4_ccr, conf->write_priority);

    omap44xx_sdma_transparent_copy_t
        transparent_copy = (conf->color_mode == omap44xx_sdma_TRANSPARENT_COPY);
    omap44xx_sdma_const_fill_t
        const_fill = (conf->color_mode == omap44xx_sdma_CONSTANT_FILL);

    dma4_ccr = omap44xx_sdma_dma4_ccr_transparent_copy_enable_insert(dma4_ccr, transparent_copy);
    dma4_ccr = omap44xx_sdma_dma4_ccr_const_fill_enable_insert(dma4_ccr, const_fill);

    return dma4_ccr;
}


static omap44xx_sdma_dma4_color_t omap_sdma_channel_conf_color(
                                omap44xx_sdma_dma4_color_t dma4_color,
                                struct omap_sdma_channel_conf *conf)
{
    // DMA4_COLORi can only be a 24 bit value
    assert((conf->color & 0xFF000000) == 0);

    return omap44xx_sdma_dma4_color_color_key_pattern_insert(dma4_color, conf->color);
}

static omap44xx_sdma_dma4_clnk_ctrl_t omap_sdma_channel_conf_clnk_ctrl(
                                omap44xx_sdma_dma4_clnk_ctrl_t dma4_clnk_ctrl,
                                struct omap_sdma_channel_conf *conf)
{
    // if we enable channel linking, the next channel has to be valid
    assert(!conf->enable_link || conf->next_channel < OMAP44XX_SDMA_NUM_CHANNEL);

    dma4_clnk_ctrl = omap44xx_sdma_dma4_clnk_ctrl_nextlch_id_insert(
        dma4_clnk_ctrl, conf->next_channel);
    dma4_clnk_ctrl = omap44xx_sdma_dma4_clnk_ctrl_enable_lnk_insert(
        dma4_clnk_ctrl, conf->enable_link ? 1 : 0);

    return dma4_clnk_ctrl;
}

static omap44xx_sdma_dma4_csdp_t omap_sdma_channel_conf_csdp(
                                omap44xx_sdma_dma4_csdp_t dma4_csdp,
                                struct omap_sdma_channel_conf *conf)
{
    assert(
        (conf->write_mode == omap44xx_sdma_WRITE_MODE_NONE_POSTED) ||
        (conf->write_mode == omap44xx_sdma_WRITE_MODE_ALL_POSTED) ||
        (conf->write_mode == omap44xx_sdma_WRITE_MODE_LAST_NON_POSTED)
    );

    dma4_csdp = omap44xx_sdma_dma4_csdp_write_mode_insert(dma4_csdp, conf->write_mode);

    // In memory to memory transfers, the endianness is always little endian
    dma4_csdp = omap44xx_sdma_dma4_csdp_src_endian_insert(dma4_csdp, omap44xx_sdma_ENDIAN_LITTLE);
    dma4_csdp = omap44xx_sdma_dma4_csdp_src_endian_lock_insert(dma4_csdp, omap44xx_sdma_ENDIAN_LOCK_ADAPT);

    dma4_csdp = omap44xx_sdma_dma4_csdp_dst_endian_insert(dma4_csdp, omap44xx_sdma_ENDIAN_LITTLE);
    dma4_csdp = omap44xx_sdma_dma4_csdp_dst_endian_lock_insert(dma4_csdp, omap44xx_sdma_ENDIAN_LOCK_ADAPT);

    struct omap_sdma_transfer_conf *src_conf = &conf->src_conf;
    dma4_csdp = omap44xx_sdma_dma4_csdp_src_burst_en_insert(dma4_csdp, src_conf->burst_mode);
    dma4_csdp = omap44xx_sdma_dma4_csdp_src_packed_insert(dma4_csdp,
        src_conf->packed_transfer
            ? omap44xx_sdma_SRC_PACKED_ENABLE
            : omap44xx_sdma_SRC_PACKED_DISABLE);

    struct omap_sdma_transfer_conf *dst_conf = &conf->dst_conf;
    dma4_csdp = omap44xx_sdma_dma4_csdp_dst_burst_en_insert(dma4_csdp, dst_conf->burst_mode);
    dma4_csdp = omap44xx_sdma_dma4_csdp_dst_packed_insert(dma4_csdp,
        dst_conf->packed_transfer
            ? omap44xx_sdma_DST_PACKED_ENABLE
            : omap44xx_sdma_DST_PACKED_DISABLE);

    struct omap_sdma_transfer_size *transfer_size = &conf->transfer_size;

    dma4_csdp = omap44xx_sdma_dma4_csdp_data_type_insert(dma4_csdp, transfer_size->data_type);

    return dma4_csdp;
}

static void inline omap_sdma_channel_conf_assert_transfer_size(
                                struct omap_sdma_transfer_size *transfer_size)
{
    assert(
        (transfer_size->data_type == omap44xx_sdma_DATA_TYPE_8BIT) ||
        (transfer_size->data_type == omap44xx_sdma_DATA_TYPE_16BIT) ||
        (transfer_size->data_type == omap44xx_sdma_DATA_TYPE_32BIT)
    );

    // element number is 24 bit
    assert((transfer_size->element_number & 0xFF000000) == 0);
}

static void inline omap_sdma_channel_conf_assert_transfer_conf(
                                struct omap_sdma_transfer_conf *transfer_conf)
{

    // constant addressing mode is not allowed for memory to memory transfers
    assert(
        (transfer_conf->addr_mode == omap44xx_sdma_ADDR_MODE_POST_INCR) ||
        (transfer_conf->addr_mode == omap44xx_sdma_ADDR_MODE_SINGLE_IDX) ||
        (transfer_conf->addr_mode == omap44xx_sdma_ADDR_MODE_DOUBLE_IDX)
    );

    assert(
        (transfer_conf->burst_mode == omap44xx_sdma_BURST_EN_SINGLE) ||
        (transfer_conf->burst_mode == omap44xx_sdma_BURST_EN_16BYTE) ||
        (transfer_conf->burst_mode == omap44xx_sdma_BURST_EN_32BYTE) ||
        (transfer_conf->burst_mode == omap44xx_sdma_BURST_EN_64BYTE)
    );

    // if post-incrementing with burst transfers is used,
    // data must be packed to DMA data-port width (TRM Section 16.4.7)
    assert(
        (transfer_conf->burst_mode == omap44xx_sdma_BURST_EN_SINGLE)    ||
        (transfer_conf->addr_mode != omap44xx_sdma_ADDR_MODE_POST_INCR) ||
        (transfer_conf->packed_transfer == omap44xx_sdma_SRC_PACKED_ENABLE)
    );
}

/**
 * \brief Configure an allocated channel with the given struct.
 *
 * \param channel   Channel to configure
 * \param conf      Complete channel configuration
 *
 * This function will write all values of the struct to the SDMA device
 * registers. Some basic santiy checks (using assert statements) are performed,
 * but is it the callers responsibility to ensure that the configuration is
 * sane and valid.
 */
void omap_sdma_set_channel_conf(omap_sdma_channel_t channel,
                                struct omap_sdma_channel_conf *conf)
{
    // check transfer config and size parameters
    assert(channel < OMAP44XX_SDMA_NUM_CHANNEL);

    omap_sdma_channel_conf_assert_transfer_size(&conf->transfer_size);

    omap_sdma_channel_conf_assert_transfer_conf(&conf->src_conf);
    omap_sdma_channel_conf_assert_transfer_conf(&conf->dst_conf);


    // Channel Control Register
    omap44xx_sdma_dma4_ccr_t dma4_ccr;
    dma4_ccr = omap44xx_sdma_dma4_ccr_rd(&devsdma, channel);
    dma4_ccr = omap_sdma_channel_conf_ccr(dma4_ccr, conf);
    omap44xx_sdma_dma4_ccr_wr(&devsdma, channel, dma4_ccr);

    // Channel Color Register
    omap44xx_sdma_dma4_color_t dma4_color;
    dma4_color = omap44xx_sdma_dma4_color_rd(&devsdma, channel);
    dma4_color = omap_sdma_channel_conf_color(channel, conf);
    omap44xx_sdma_dma4_color_wr(&devsdma, channel, dma4_color);

    // Channel Link Control Register
    omap44xx_sdma_dma4_clnk_ctrl_t dma4_clnk_ctrl;
    dma4_clnk_ctrl = omap44xx_sdma_dma4_clnk_ctrl_rd(&devsdma, channel);
    dma4_clnk_ctrl = omap_sdma_channel_conf_clnk_ctrl(channel, conf);
    omap44xx_sdma_dma4_clnk_ctrl_wr(&devsdma, channel, dma4_clnk_ctrl);

    // Channel Source Destination Parameters
    omap44xx_sdma_dma4_csdp_t dma4_csdp;
    dma4_csdp = omap44xx_sdma_dma4_csdp_rd(&devsdma, channel);
    dma4_csdp = omap_sdma_channel_conf_csdp(channel, conf);
    omap44xx_sdma_dma4_csdp_wr(&devsdma, channel, dma4_csdp);

    // Channel Element Number
    omap44xx_sdma_dma4_cen_wr(&devsdma, channel, conf->transfer_size.element_number);

    // Channel Frame Number
    omap44xx_sdma_dma4_cfn_wr(&devsdma, channel, conf->transfer_size.frame_number);

    // Channel Source Element Index
    omap44xx_sdma_dma4_csei_wr(&devsdma, channel, conf->src_conf.element_index);
    // Channel Source Frame Index
    omap44xx_sdma_dma4_csfi_wr(&devsdma, channel, conf->src_conf.frame_index);
    // Channel Destination Element Index
    omap44xx_sdma_dma4_cdei_wr(&devsdma, channel, conf->dst_conf.element_index);
    // Channel Destination Frame Index
    omap44xx_sdma_dma4_cdfi_wr(&devsdma, channel, conf->dst_conf.frame_index);

    // Channel Source Start Address
    omap44xx_sdma_dma4_cssa_wr(&devsdma, channel, conf->src_conf.start_address);

    // Channel Source Destination Address
    omap44xx_sdma_dma4_cdsa_wr(&devsdma, channel, conf->dst_conf.start_address);
}

/**
 * \brief Start a SDMA transfer on a pre-configured channel
 *
 * \param channel   Pre-configured channel to enable
 * \param interrupt Indicate an interrupt should be sent on completion or error
 *
 * It is the callers responsibility to ensure that the channel was configured
 * properly before this function is called.
 */
void omap_sdma_enable_channel(omap_sdma_channel_t channel, bool interrupt)
{
    assert(channel < OMAP44XX_SDMA_NUM_CHANNEL);

    // clear all channel status flags
    omap44xx_sdma_dma4_csr_wr(&devsdma, channel, 0xFFFFFFFF);

    uint32_t irqenable = omap44xx_sdma_dma4_irqenable_rd(&devsdma, OMAP44XX_SDMA_IRQ_LINE);
    if (interrupt) {
        // set channel
        irqenable |= 1 << channel;
        // reset irq status for this channel
        omap44xx_sdma_dma4_irqstatus_line_wr(&devsdma, OMAP44XX_SDMA_IRQ_LINE, 1 << channel);
    } else {
        // clear channel
        irqenable &= ~(1 << channel);
    }
    omap44xx_sdma_dma4_irqenable_wr(&devsdma, OMAP44XX_SDMA_IRQ_LINE, irqenable);

    omap44xx_sdma_dma4_ccr_enable_wrf(&devsdma, channel, 1);
}

/**
 * \brief Poll a enabled channel for completion of the transfer.
 */
errval_t omap_sdma_poll_channel(omap_sdma_channel_t channel)
{
    assert(channel < OMAP44XX_SDMA_NUM_CHANNEL);

    // interrupts should be disabled in polling mode
    assert((omap44xx_sdma_dma4_irqenable_rd(
                &devsdma, OMAP44XX_SDMA_IRQ_LINE) & 1 << channel) == 0x0
    );

    for (;;) {
        omap44xx_sdma_dma4_csr_t csr = omap44xx_sdma_dma4_csr_rd(&devsdma, channel);

        errval_t err = omap_sdma_read_csr(csr);

        if (err_is_fail(err)) return err;

        if(omap44xx_sdma_dma4_csr_block_extract(csr)) {
            return err;
        }
    }
}

/**
 * \brief Allocate a SDMA channel. Will return an error if there are no channels
 * available.
 */
errval_t omap_sdma_allocate_channel(omap_sdma_channel_t *channel)
{
    assert(channel != NULL);

    for (omap_sdma_channel_t c = 0; c<OMAP44XX_SDMA_NUM_CHANNEL; c++) {
        if (!allocated_channel[c]) {
            allocated_channel[c] = true;
            *channel = c;
            return SYS_ERR_OK;
        }
    }
    return OMAP_SDMA_ERR_NO_AVAIL_CHANNEL;
}

/**
 * \brief Frees a previously allocated SDMA channel.
 */
void omap_sdma_free_channel(omap_sdma_channel_t channel)
{
    assert(allocated_channel[channel]);
    allocated_channel[channel] = false;
}

/**
 * \brief Initializes this Mackerel wrapper.
 *
 * \param dev_base  Virtual address where the SDMA module is mapped to
 * \param irq_cb    Mandatory interrupt callback
 *
 * The caller has map the SDMA hardware registers before calling this function.
 * The interrupt callback is executed for every time an SDMA channel triggered
 * an interrupt. The source channel and the the reason of the interrupt are
 * passed to the callback as an the argument.
 */
errval_t omap_sdma_init(mackerel_addr_t dev_base, omap_sdma_irq_handler_t irq_cb)
{
    // init global variables
    STATIC_ASSERT_SIZEOF(bool, 1);
    memset(allocated_channel, false, OMAP44XX_SDMA_NUM_CHANNEL);

    omap44xx_sdma_initialize(&devsdma, dev_base);

    // check if we can read the revision
    assert(omap44xx_sdma_dma4_revision_rd(&devsdma) == 0x10900);

    assert(irq_cb != NULL);
    irq_callback = irq_cb;

    errval_t err = SYS_ERR_OK;
#ifdef OMAP_SDMA_KERNELBENCH
    // in kernelspace, we hijack the normal interrupt handler by overwriting
    // the global interrupt handler table entry.

    // this is obvioulsy a hack (!) and ONLY for use in the sdma kernel
    // benchmark which will not allow the kernel to continue after this
    uintptr_t *irq_handler_entry = (uintptr_t*)
                        (ETABLE_ADDR + JUMP_TABLE_OFFSET + ARM_EVECTOR_IRQ);
    *irq_handler_entry = (uintptr_t) omap_sdma_kernel_irq_handler;

    // enable the SDMA interrupt in the global interrupt controller
    gic_enable_interrupt(OMAP44XX_SDMA_IRQ, GIC_IRQ_CPU_TRG_ALL, 0,
                            GIC_IRQ_EDGE_TRIGGERED, GIC_IRQ_N_TO_N);
#else
    // in userspace, register normal interrupt handler
    err = inthandler_setup_arm(omap_sdma_irq_handler, NULL, OMAP44XX_SDMA_IRQ);
#endif

    // set fifo depth to maximum burst size
    omap44xx_sdma_dma4_gcr_max_channel_fifo_depth_wrf(&devsdma, 64);

    // configure error and interrupt handling of the device
    for(omap_sdma_channel_t channel = 0;
        channel < OMAP44XX_SDMA_NUM_CHANNEL;
        channel++)
    {
        omap_sdma_irq_config(channel);
    }

    return err;
}
