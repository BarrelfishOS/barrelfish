/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <stdio.h>
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>

#include <thc/thc.h>

#include <arch/arm/omap44xx/device_registers.h>

#include "sdma.h"
#include "omap_sdma.h"

// Channel State. Filled by the interrupt callback, read by the request task.
static struct {
    awe_t *request;
    errval_t err;
} channel_state[OMAP44XX_SDMA_NUM_CHANNEL];

/**
 * \brief Interrupt callback which will be called when a channel interrupt
 * occurs.
 *
 * \param channel   Channel which triggered the interrupt
 * \param err       State of the channel, SYS_ERR_OK if transfer completed
 */
static void sdma_irq_handler(omap_sdma_channel_t channel, errval_t err)
{
    channel_state[channel].err = err;
    THCSchedule(channel_state[channel].request);
}

/**
 * \brief Execute a transfer on the SDMA engine. Blocks until the transfer is
 * completed or an error occurred.
 *
 * \param conf   Pointer to valid & initialized channel configuration
 */
static errval_t run_omap_sdma_transfer(struct omap_sdma_channel_conf *conf)
{
    errval_t err;
    omap_sdma_channel_t channel;

    err = omap_sdma_allocate_channel(&channel);
    if (err_is_fail(err)) return err;

    // configure and enable allocated channel
    omap_sdma_set_channel_conf(channel, conf);
    omap_sdma_enable_channel(channel, true);

    // this task will be rescheduled by the IRQ handler
    THCSuspend(&channel_state[channel].request);

    // read status flag set by IRQ handler
    err = channel_state[channel].err;

    omap_sdma_free_channel(channel);

    return err;
}

/**
 * \brief Converts the pixel size of the Flounder interface description to the
 * element size needed for the hardware.
 *
 */
static omap44xx_sdma_data_type_t extract_data_type(omap_sdma_data_type_t pixel_size)
{
    omap44xx_sdma_data_type_t data_type;

    switch(pixel_size) {
    case omap_sdma_DATA_TYPE_8BIT:
        data_type = omap44xx_sdma_DATA_TYPE_8BIT;
        break;
    case omap_sdma_DATA_TYPE_16BIT:
        data_type = omap44xx_sdma_DATA_TYPE_16BIT;
        break;
    case omap_sdma_DATA_TYPE_32BIT:
    default:
        data_type = omap44xx_sdma_DATA_TYPE_32BIT;
        break;
    }

    return data_type;
}

/**
 * \brief Initializes a configuration struct for the given parameters. It is
 * the callers responsibility ensure that the start address and count values
 * are valid.
 */
static void init_channel_conf(struct omap_sdma_channel_conf *conf,
                lpaddr_t dst_start, lpaddr_t src_start,
                int32_t dst_x_modify, int32_t dst_y_modify,
                int32_t src_x_modify, int32_t src_y_modify,
                omap_sdma_count_2d_t count,
                omap44xx_sdma_color_mode_t color_mode, uint32_t color)
{
    assert(conf);

    omap44xx_sdma_data_type_t data_type = extract_data_type(count.pixel_size);

    // OMAP4460 TRM: SDMA 16.4.5
    int32_t es = 1 << (data_type);
    int32_t src_element_index = (src_x_modify - 1) * es + 1;
    int32_t src_frame_index   = (src_y_modify - 1) * es + 1;

    int32_t dst_element_index = (dst_x_modify - 1) * es + 1;
    int32_t dst_frame_index   = (dst_y_modify - 1) * es + 1;

    *conf = (struct omap_sdma_channel_conf) {
        // low priority for software-synchronized transfers
        .read_priority  = omap44xx_sdma_PORT_PRIORITY_LOW,
        .write_priority = omap44xx_sdma_PORT_PRIORITY_LOW,

        // normal copy/transparent copy/constant fill
        .color_mode = color_mode,
        .color = color,

        // wait for last write to complete
        .write_mode = omap44xx_sdma_WRITE_MODE_LAST_NON_POSTED,

        // channel linking is not used
        .enable_link = false,
        .next_channel = 0,

        // always use double indexing mode, packed & burst transfer
        .src_conf = {
            .start_address      = src_start,
            .addr_mode          = omap44xx_sdma_ADDR_MODE_DOUBLE_IDX,
            .element_index      = src_element_index,
            .frame_index        = src_frame_index,
            .packed_transfer    = omap44xx_sdma_SRC_PACKED_ENABLE,
            .burst_mode         = omap44xx_sdma_BURST_EN_64BYTE,
        },

        .dst_conf = {
            .start_address      = dst_start,
            .addr_mode          = omap44xx_sdma_ADDR_MODE_DOUBLE_IDX,
            .element_index      = dst_element_index,
            .frame_index        = dst_frame_index,
            .packed_transfer    = omap44xx_sdma_DST_PACKED_ENABLE,
            .burst_mode         = omap44xx_sdma_BURST_EN_64BYTE,
        },

        // conversion of omap_count_2d
        .transfer_size = {
            .element_number = count.x_count,
            .frame_number   = count.y_count,
            .data_type      = data_type,
        },
    };
}

/**
 * \brief Splits the physical frame size into two factors, as the SDMA engine
 * needs the memory region to be specified in EN * FN.
 *
 * \param bits      Size of the frame as a power of two.
 * \param retcount  Pointer to count struct which will be filled with frame size
 */
static void init_count_1d(uint8_t bits, omap_sdma_count_2d_t *retcount)
{
    assert(retcount);

    // split frame size: 2^b = 2^(b/2) * 2^(b - b/2)
    uint8_t x_bits = MIN(bits, OMAP44XX_SDMA_MAX_EN_BITS);
    uint8_t y_bits = bits - x_bits;

    // fill count struct
    retcount->pixel_size = omap_sdma_DATA_TYPE_8BIT;
    retcount->x_count = 1 << x_bits;
    retcount->y_count = 1 << y_bits;
}

/**
 * \brief Performs a 32bit integer multiplication and checks for overflow.
 */
inline static bool i32_mull_overflow(int32_t y, int32_t x, int32_t* prod) {
    int64_t i64prod=(int64_t)x*y;
    if (i64prod > INT32_MAX || i64prod < INT32_MIN) return true;
    *prod = i64prod & 0xffffffff;
    return false;
}

/**
 * \brief Calculates the start address for a given frame capability in a
 * two-dimensional transfer.
 *
 * \param cap       frame capability in which the transfer should happen
 * \param addr      addr_2d struct containing start offset and modifiers
 * \param count     count_2d struct specifing size of the transfer
 * \param retaddr   filled with the physical start address of the transfer
 *
 * This function also does some sanity checks, to ensure that the hardware will
 * not access any values outside the frame boundaries.
 */
static errval_t frame_address_2d(struct capref cap, omap_sdma_addr_2d_t *addr,
                omap_sdma_count_2d_t *count, lpaddr_t *retaddr)
{
    assert(addr);
    assert(count);
    assert(retaddr);

    errval_t err;
    struct frame_identity id;

    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) return err_push(err, OMAP_SDMA_ERR_CAP_LOOKUP);

    lpaddr_t frame_start = id.base;
    int32_t frame_size = (1 << id.bits);

    // image size cannot exceed hardware limits
    if (count->x_count > OMAP44XX_SDMA_MAX_EN ||
        count->y_count > OMAP44XX_SDMA_MAX_FN
    ) {
        return OMAP_SDMA_ERR_HARDWARE_LIMIT_SIZE;
    }

    // pixel size in bytes
    int32_t pixel_size = 1 << extract_data_type(count->pixel_size);
    // image size in pixels
    int32_t x_cnt = count->x_count;
    int32_t y_cnt = count->y_count;

    // {x,y} modifiers and their accumulated value
    // (all value in bytes, not pixels!)
    int32_t x_mod, y_mod,
            x_mod_sum, y_mod_sum;

    // x_mod = addr->x_modify * pixel_size
    // y_mod = addr->y_modify * pixel_size
    // x_mod_sum = (x_cnt-1) * x_mod;
    // y_mod_sum = (y_cnt-1) * y_mod;

    // check for integer overflow
    if (
        (addr->x_modify > INT16_MAX || addr->x_modify < INT16_MIN) ||
        i32_mull_overflow(addr->x_modify, pixel_size, &x_mod) ||
        i32_mull_overflow(addr->y_modify, pixel_size, &y_mod) ||
        i32_mull_overflow(x_cnt-1, x_mod, &x_mod_sum) ||
        i32_mull_overflow(y_cnt-1, y_mod, &y_mod_sum)
    ) {
        return OMAP_SDMA_ERR_HARDWARE_LIMIT_ADDR;
    }

    // first access performed by the device (start offset)
    int32_t first_access = (addr->y_start * y_cnt + addr->x_start) * pixel_size;
    // last access performed by the device
    int32_t last_access  = first_access + (y_cnt * x_mod_sum) + y_mod_sum;

    int32_t lowest_access, highest_access;

    if (x_mod >= 0 && y_mod >= 0) {
        // monotonic access
        // first access is smallest, last access is largest
        lowest_access  = first_access;
        highest_access = last_access;
    } else if (x_mod < 0 && y_mod < 0) {
        // monotonic access
        // last access is smallest, first access is largest
        lowest_access  = last_access;
        highest_access = first_access;
    } else {
        // non-monotonic access
        if (x_mod > 0) {
            // x_mod > 0, y_mod < 0
            if (x_mod_sum + y_mod < 0) {
                lowest_access  = last_access  - x_mod_sum;
                highest_access = first_access + x_mod_sum;
            } else {
                lowest_access  = first_access;
                highest_access = last_access;
            }
        } else {
            // x_mod < 0, y_mod > 0
            if (x_mod_sum + y_mod > 0) {
                lowest_access  = first_access + x_mod_sum;
                highest_access = last_access  - x_mod_sum;
            } else {
                lowest_access  = last_access;
                highest_access = first_access;
            }
        }
    }

    // all accesses have to be within frame boundaries
    if (lowest_access < 0 || highest_access >= frame_size) {
        return OMAP_SDMA_ERR_OUT_OF_BOUNDS;
    }

    *retaddr = frame_start + first_access;

    return SYS_ERR_OK;
}

/**
 * \brief Stub to perform simple frame-to-frame memory copy
 * \see   Flounder definition in if/omap_sdma.if
 */
errval_t mem_copy(struct capref dst_cap, struct capref src_cap)
{
    errval_t err;
    omap_sdma_count_2d_t count;
    struct frame_identity src_id, dst_id;

    // get frame sizes
    err = invoke_frame_identify(src_cap, &src_id);
    if (err_is_fail(err)) return err_push(err, OMAP_SDMA_ERR_CAP_LOOKUP);

    err = invoke_frame_identify(dst_cap, &dst_id);
    if (err_is_fail(err)) return err_push(err, OMAP_SDMA_ERR_CAP_LOOKUP);

    // infer element/frame number for smaller frame
    init_count_1d(MIN(dst_id.bits, dst_id.bits), &count);

    // configure and initiate transfer
    struct omap_sdma_channel_conf conf;
    init_channel_conf(&conf, dst_id.base, src_id.base, 1, 1, 1, 1, count,
                         omap44xx_sdma_DISABLE_COLOR_MODE, 0);
    err = run_omap_sdma_transfer(&conf);

    return err;
}

/**
 * \brief Stub to fill a memory frame with a constant value
 * \see   Flounder definition in if/omap_sdma.if
 */
errval_t mem_fill(struct capref dst_cap, uint8_t color)
{
    errval_t err;
    omap_sdma_count_2d_t count;
    struct frame_identity dst_id;

    // get frame size and infer element/frame number
    err = invoke_frame_identify(dst_cap, &dst_id);
    if (err_is_fail(err)) return err_push(err, OMAP_SDMA_ERR_CAP_LOOKUP);
    init_count_1d(dst_id.bits, &count);

    // configure and initiate transfer
    struct omap_sdma_channel_conf conf;
    init_channel_conf(&conf, dst_id.base, 0, 1, 1, 1, 1, count,
                         omap44xx_sdma_CONSTANT_FILL, color);
    err = run_omap_sdma_transfer(&conf);

    return err;
}

/**
 * \brief Stub to perform a two-dimensional memory copy
 * \see   Flounder definition in if/omap_sdma.if
 */
errval_t mem_copy_2d(omap_sdma_addr_2d_t dst, omap_sdma_addr_2d_t src,
                omap_sdma_count_2d_t count, bool transparent, uint32_t color)
{
    errval_t err;
    lpaddr_t src_start, dst_start;

    // check boundaries and calculate start address for source/dest frames
    err = frame_address_2d(dst.cap, &dst, &count, &dst_start);
    if (err_is_fail(err)) return err;

    err = frame_address_2d(src.cap, &src, &count, &src_start);
    if (err_is_fail(err)) return err;

    // use transparent copy mode if requested
    omap44xx_sdma_color_mode_t color_mode = (transparent) ?
                                                omap44xx_sdma_TRANSPARENT_COPY :
                                                omap44xx_sdma_DISABLE_COLOR_MODE;

    struct omap_sdma_channel_conf conf;
    init_channel_conf(&conf, dst_start, src_start,
                         dst.x_modify, dst.y_modify,
                         src.x_modify, src.y_modify,
                         count, color_mode, color);

    err = run_omap_sdma_transfer(&conf);
    return err;
}

/**
 * \brief Stub to fill parts of a frame using two-dimensional indeces
 * \see   Flounder definition in if/omap_sdma.if
 */
errval_t mem_fill_2d(omap_sdma_addr_2d_t dst, omap_sdma_count_2d_t count, uint32_t color)
{
    errval_t err;
    lpaddr_t  dst_start;

    err = frame_address_2d(dst.cap, &dst, &count, &dst_start);
    if (err_is_fail(err)) return err;

    struct omap_sdma_channel_conf conf;
    init_channel_conf(&conf, dst_start, 0,
                         dst.x_modify, dst.y_modify, 0, 0,
                         count, omap44xx_sdma_CONSTANT_FILL, color);

    err = run_omap_sdma_transfer(&conf);
    return err;
}

int main(int argc, char **argv)
{
    errval_t err;
    lvaddr_t dev_base;

    err = map_device_register(OMAP44XX_SDMA, 0x1000, &dev_base);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to map SDMA registers");
    }

    omap_sdma_init((mackerel_addr_t)dev_base, sdma_irq_handler);
    start_service();

    return 0;
}
