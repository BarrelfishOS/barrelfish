/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

#include <thc/thc.h>

#include <if/omap_sdma_defs.h>
#include <if/omap_sdma_thc.h>

#include <string.h>

#define MAX_FRAME_SIZE (32UL*1024*1024)

static struct capref src_frame, dst_frame;
static uint8_t *src_buf, *dst_buf;

extern char logo_data[101200];

static void allocate_and_map_frames(void)
{
    errval_t err;
    size_t retbytes;

    err = frame_alloc(&src_frame, MAX_FRAME_SIZE, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes >= MAX_FRAME_SIZE);

    err = frame_alloc(&dst_frame, MAX_FRAME_SIZE, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes >= MAX_FRAME_SIZE);

    err = vspace_map_one_frame_attr((void**)&src_buf, MAX_FRAME_SIZE, src_frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    assert(err_is_ok(err));

    err = vspace_map_one_frame_attr((void**)&dst_buf, MAX_FRAME_SIZE, dst_frame,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    assert(err_is_ok(err));
}

static void fill_mem(void *buf, size_t bytesize, bool iterate, uint32_t val)
{
    assert(buf);
    assert(bytesize % sizeof(uint32_t) == 0);

    uint32_t *fillbuf = buf;
    size_t size = bytesize / sizeof(uint32_t);

    for(size_t i=0; i<size; i++) {
        fillbuf[i] = (iterate) ? i : val;
    }
}

static void test_mem(void *buf, size_t bytesize, bool iterate, uint32_t val)
{
    assert(buf);
    assert(bytesize % sizeof(uint32_t) == 0);

    uint32_t *fillbuf = buf;
    size_t size = bytesize / sizeof(uint32_t);

    for(size_t i=0; i<size; i++) {
        uint32_t expected = (iterate) ? i : val;
        if(fillbuf[i] != expected) {
            USER_PANIC("Invalid word 0x%08x at offset %zu, expected 0x%08x\n",
                        fillbuf[i], i, expected);
        }
    }
}

static void rotate_16bit_image(uint16_t *dst, uint16_t *src,
                         omap_sdma_addr_2d_t addr,
                         omap_sdma_count_2d_t count)
{
    assert(dst && src);
    assert(count.pixel_size == omap_sdma_DATA_TYPE_16BIT);

    dst += addr.x_start + (count.y_count * addr.y_start);
    for (size_t y = 1; y <= count.y_count; y++) {
        for (size_t x = 1; x <= count.x_count; x++) {
            *dst = *(src++);
            if (x < count.x_count) {
                // within the frame
                dst += addr.x_modify;
            } else {
                // at the end of a frame
                dst += addr.y_modify;
            }
        }
    }
}

static void transcopy_16bit_image(uint16_t *dst, uint16_t *src,
                         omap_sdma_count_2d_t count,
                         uint16_t color, uint32_t replacement)
{
    assert(dst && src);
    assert(count.pixel_size == omap_sdma_DATA_TYPE_16BIT);

    for (size_t y = 1; y <= count.y_count; y++) {
        for (size_t x = 1; x <= count.x_count; x++) {
            *(dst) = (*src != color) ? *src : replacement;

            dst++; src++;
        }
    }
}

static void run_client(struct omap_sdma_thc_client_binding_t *cl)
{
    errval_t err;

    debug_printf("mem_copy test\n");

    ////////// Test simple memory copying of whole frame //////////

    fill_mem(src_buf, MAX_FRAME_SIZE, true, 0);
    fill_mem(dst_buf, MAX_FRAME_SIZE, false, 0);
    cl->call_seq.mem_copy(cl, dst_frame, src_frame, &err);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "omap mem_copy");
    }
    test_mem(dst_buf, MAX_FRAME_SIZE, true, 0);

    ////////// Test simple memory filling of whole frame //////////

    debug_printf("mem_fill test\n");

    cl->call_seq.mem_fill(cl, dst_frame, 0xAB, &err);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "omap mem_fill");
    }
    test_mem(dst_buf, MAX_FRAME_SIZE, false, 0xABABABAB);

    ////////// 2D Simple Transfer Test //////////

    debug_printf("mem_copy_2d test\n");

    STATIC_ASSERT(MAX_FRAME_SIZE >= sizeof(logo_data), "frame too small");
    memcpy(src_buf, logo_data, sizeof(logo_data));

    omap_sdma_count_2d_t count_2d = {
        .pixel_size = omap_sdma_DATA_TYPE_16BIT,
        .x_count = 230,
        .y_count = 220,
    };

    omap_sdma_addr_2d_t src_addr = {
        .cap = src_frame,
        .x_start = 0,
        .y_start = 0,
        .x_modify = 1,
        .y_modify = 1,
    };

    omap_sdma_addr_2d_t dst_addr = src_addr;
    dst_addr.cap = dst_frame;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "omap mem_copy_2d");
    } else if (memcmp(dst_buf, logo_data, sizeof(logo_data)) != 0) {
        USER_PANIC("dst buffer did not match image\n");
    }

    ////////// 2D Rotation Test //////////

    debug_printf("mem_copy_2d rotation test\n");

    dst_addr.cap = dst_frame;
    dst_addr.x_start = count_2d.y_count-1;  // start in top right conrner
    dst_addr.y_start = 0;
    dst_addr.x_modify = count_2d.y_count;   // jump down one row
    dst_addr.y_modify = -((count_2d.x_count-1) * count_2d.y_count + 1);

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);

    // rotate image in software for comparision
    uint16_t *rot_data = malloc(sizeof(logo_data));
    rotate_16bit_image(rot_data, (uint16_t*)logo_data, dst_addr, count_2d);

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "omap mem_copy_2d");
    } else if (memcmp(dst_buf, rot_data, sizeof(logo_data)) != 0) {
        USER_PANIC("dst buffer did not match image\n");
    }

    free(rot_data);

    ////////// 2D Constant Fill //////////

    debug_printf("mem_fill_2d test\n");

    // reset destination addressing mode
    dst_addr = src_addr;
    dst_addr.cap = dst_frame;

    cl->call_seq.mem_fill_2d(cl, dst_addr, 0x4242, count_2d, &err);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "omap mem_fill_2d");
    }
    test_mem(dst_buf, sizeof(logo_data), false, 0x42424242);

    ////////// 2D Transparent Copy //////////

    debug_printf("mem_copy_2d transparent copy test\n");

    dst_addr = src_addr;
    dst_addr.cap = dst_frame;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr,
                             count_2d, true, 0xFFFF, &err);

    // perform transparent copy in software for comparision
    uint16_t *trans_data = malloc(sizeof(logo_data));
    transcopy_16bit_image(trans_data, (uint16_t*)logo_data,
                          count_2d, 0xFFFF, 0x4242);

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "omap mem_copy_2d");
    } else if (memcmp(dst_buf, trans_data, sizeof(logo_data)) != 0) {
        USER_PANIC("dst buffer did not match image\n");
    }

    free(trans_data);

    ////////// Invalid Capability Test //////////

    debug_printf("mem_copy invalid cap test\n");

    cl->call_seq.mem_fill(cl, NULL_CAP, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_CAP_LOOKUP) {
        USER_PANIC_ERR(err, "invalid capability lookup\n");
    }

    ////////// Invalid Address Modifier Test //////////

    debug_printf("mem_copy_2d invalid address modifier test\n");

    dst_addr.x_modify = 0x10000;
    dst_addr.y_modify = 1;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_HARDWARE_LIMIT_ADDR) {
        USER_PANIC_ERR(err, "oversized x_modify was not detected\n");
    }

    dst_addr.x_modify = 1;
    dst_addr.y_modify = INT32_MIN;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_HARDWARE_LIMIT_ADDR) {
        USER_PANIC_ERR(err, "invalid dst y_modify was not detected\n");
    }

    src_addr.x_modify = 1;
    src_addr.y_modify = INT32_MAX;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_HARDWARE_LIMIT_ADDR) {
        USER_PANIC_ERR(err, "invalid src y_modify was not detected\n");
    }

    ////////// Invalid Frame Size Test //////////

    debug_printf("mem_copy_2d invalid frame size test\n");

    src_addr.x_modify = src_addr.y_modify = 1;
    dst_addr.x_modify = dst_addr.y_modify = 1;

    count_2d.x_count = 0x1000000;
    count_2d.y_count = 1;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_HARDWARE_LIMIT_SIZE) {
        USER_PANIC_ERR(err, "invalid x_count was not detected\n");
    }

    count_2d.y_count = 0x10000;
    count_2d.x_count = 1;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_HARDWARE_LIMIT_SIZE) {
        USER_PANIC_ERR(err, "invalid y_count was not detected\n");
    }

    count_2d.y_count = -1;
    count_2d.x_count = -1;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_HARDWARE_LIMIT_SIZE) {
        USER_PANIC_ERR(err, "negative count was not detected\n");
    }

    ////////// Out of Bounds Access Test //////////

    debug_printf("mem_copy_2d out of bounds test\n");

    count_2d.pixel_size = omap_sdma_DATA_TYPE_32BIT;
    count_2d.x_count = MAX_FRAME_SIZE / 1024 / 4;
    count_2d.y_count = 1024 + 1;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_OUT_OF_BOUNDS) {
        USER_PANIC_ERR(err, "out of bound access was not detected\n");
    }

    count_2d.pixel_size = omap_sdma_DATA_TYPE_8BIT;
    count_2d.x_count = MAX_FRAME_SIZE / 1024;
    count_2d.y_count = 1024;

    dst_addr.x_modify = dst_addr.y_modify = -1;

    cl->call_seq.mem_copy_2d(cl, dst_addr, src_addr, count_2d, false, 0, &err);
    if (err_no(err) != OMAP_SDMA_ERR_OUT_OF_BOUNDS) {
        USER_PANIC_ERR(err, "out of bound access was not detected\n");
    }

    debug_printf("finished all tests successfully\n");
}

static void start_client(void)
{
    errval_t err;

    struct omap_sdma_binding *b;
    struct omap_sdma_thc_client_binding_t *cl;

    err = omap_sdma_thc_connect_by_name("sdma",
                                      get_default_waitset(),
                                      IDC_BIND_FLAGS_DEFAULT,
                                      &b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not bind (thc)");
    }

    cl = malloc(sizeof(struct omap_sdma_thc_client_binding_t));
    assert(cl != NULL);

    err = omap_sdma_thc_init_client(cl, b, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not init client (thc)");
    }

    run_client(cl);

    free(cl);
}

int main(int argc, char *argv[])
{
    allocate_and_map_frames();
    start_client();

    return 0;
}
