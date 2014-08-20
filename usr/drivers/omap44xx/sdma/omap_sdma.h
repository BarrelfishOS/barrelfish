/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef OMAP44XX_SDMA_H_
#define OMAP44XX_SDMA_H_

#include <stdbool.h>
#include <dev/omap/omap44xx_sdma_dev.h>

typedef uint8_t omap_sdma_channel_t;

typedef void (*omap_sdma_irq_handler_t)(omap_sdma_channel_t, errval_t);

typedef uint8_t omap44xx_sdma_color_mode_t;
#define omap44xx_sdma_DISABLE_COLOR_MODE ((omap44xx_sdma_color_mode_t)0x0)
#define omap44xx_sdma_TRANSPARENT_COPY   ((omap44xx_sdma_color_mode_t)0x1)
#define omap44xx_sdma_CONSTANT_FILL      ((omap44xx_sdma_color_mode_t)0x2)

#define OMAP44XX_SDMA_IRQ_LINE (0)
#define OMAP44XX_SDMA_NUM_CHANNEL (32u)
#define OMAP44XX_SDMA_IRQ (32 + 12 + OMAP44XX_SDMA_IRQ_LINE) // MA_IRQ_12

#define OMAP44XX_SDMA_MAX_FN    0x0000FFFF
#define OMAP44XX_SDMA_MAX_EN    0x00FFFFFF

#define OMAP44XX_SDMA_MAX_FN_BITS    15
#define OMAP44XX_SDMA_MAX_EN_BITS    23

#ifdef SDMA_DEBUG
#define SDMA_PRINT(...) do{ printf(__VA_ARGS__ ); } while( false )
#else
#define SDMA_PRINT(...) do{ } while ( false )
#endif

struct omap_sdma_transfer_conf {
    lpaddr_t start_address;
    omap44xx_sdma_addr_mode_t addr_mode;
    int16_t element_index;
    int32_t frame_index;
    bool packed_transfer;
    omap44xx_sdma_burst_en_t burst_mode;
};

struct omap_sdma_transfer_size {
    uint32_t element_number;
    uint16_t frame_number;
    omap44xx_sdma_data_type_t data_type;
};

/// The values in this struct are directly written into the hardware registers.
/// While there are some basic sanity checks, it is the users responsibility to
/// ensure that writes to these physical addresses are allowed.
///
/// The naming follows more or less Chapter 16 of the OMAP4466 TRM (Rev. AA)

struct omap_sdma_channel_conf {
    omap44xx_sdma_port_priority_t read_priority;
    omap44xx_sdma_port_priority_t write_priority;

    omap44xx_sdma_color_mode_t color_mode;
    uint32_t color;

    omap44xx_sdma_write_mode_t write_mode;

    struct omap_sdma_transfer_size transfer_size;
    struct omap_sdma_transfer_conf src_conf;
    struct omap_sdma_transfer_conf dst_conf;

    bool enable_link;
    omap_sdma_channel_t next_channel;
};

errval_t omap_sdma_init(mackerel_addr_t dev_base, omap_sdma_irq_handler_t);

errval_t omap_sdma_allocate_channel(omap_sdma_channel_t *channel);
void omap_sdma_free_channel(omap_sdma_channel_t channel);

void omap_sdma_init_channel_conf(struct omap_sdma_channel_conf *conf);

void omap_sdma_set_channel_conf(omap_sdma_channel_t channel,
    struct omap_sdma_channel_conf *conf);

void omap_sdma_enable_channel(omap_sdma_channel_t channel, bool interrupt);

errval_t omap_sdma_poll_channel(omap_sdma_channel_t channel);

#endif // OMAP44XX_SDMA_H_
