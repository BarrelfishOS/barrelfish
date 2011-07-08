/**
 * \file
 * \brief VBE driver service handling.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <pci/pci.h>
#include <if/fb_defs.h>

#include "vbe.h"

#define MAX_MODES       128

struct mode {
    int         xres, yres, bpp;
    uint16_t    mode;
};

static const char *service_name = "framebuffer";
static int nmodes = 0;
static struct mode modes[MAX_MODES];

static void vbe_get_framebuffer(struct fb_binding *b)
{
    struct capref fbcap;

    int r = vbe_get_framebuffer_cap(&fbcap);
    assert(r == 0);

    b->tx_vtbl.get_framebuffer_response(b, NOP_CONT, fbcap);
}

static void vbe_set_videomode(struct fb_binding *b, int xres, int yres, int bpp)
{
    int error = 1;

    for(int i = 0; i < MAX_MODES; i++) {
        if(xres == modes[i].xres && yres == modes[i].yres &&
           bpp == modes[i].bpp) {
            uint32_t r = vbe_setmode(modes[i].mode, true, false);
            assert(r == VBE_OK);
            error = 0;
            break;
        }
    }

    b->tx_vtbl.set_videomode_response(b, NOP_CONT, error);
}

static void get_vesamode(struct fb_binding *b)
{
    uint16_t mode = 0;
    bool linear = 0;
    uint32_t r = vbe_getmode(&mode, &linear);

    if (r == VBE_OK) {
        r = 0; // bleh!
    }

    b->tx_vtbl.get_vesamode_response(b, NOP_CONT, mode, linear, r);
}

static void set_vesamode(struct fb_binding *b, uint16_t mode,
                         int linear, int clear)
{
    uint32_t r = vbe_setmode(mode, linear, clear);

    if (r == VBE_OK) {
        r = 0; // bleh!
    }

    b->tx_vtbl.set_vesamode_response(b, NOP_CONT, r);
}

static void save_vesastate(struct fb_binding *b)
{
    uint32_t r = vbe_savestate();

    if(r == VBE_OK) {
        r = 0;
    }

    b->tx_vtbl.save_vesastate_response(b, NOP_CONT, r);
}

static void restore_vesastate(struct fb_binding *b)
{
    uint32_t r = vbe_restorestate();

    if(r == VBE_OK) {
        r = 0;
    }

    b->tx_vtbl.restore_vesastate_response(b, NOP_CONT, r);
}

static void vsync_handler(struct fb_binding *b)
{
    vbe_vsync();
    b->tx_vtbl.vsync_response(b, NOP_CONT);
}

static struct fb_rx_vtbl fb_rx_vtbl = {
    .get_framebuffer_call = vbe_get_framebuffer,
    .set_videomode_call = vbe_set_videomode,
    .get_vesamode_call = get_vesamode,
    .set_vesamode_call = set_vesamode,
    .save_vesastate_call = save_vesastate,
    .restore_vesastate_call = restore_vesastate,
    .vsync_call = vsync_handler,
};

static errval_t connect_cb(void *st, struct fb_binding *b)
{
    b->rx_vtbl = fb_rx_vtbl;
    return SYS_ERR_OK;
}

static void listen_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    err = nameservice_register(service_name, iref);
    assert(err_is_ok(err));
}

void vbe_driver_init_done(void)
{
    struct vbeinfoblock vib;
    uint16_t *mde = vbe_getmodes();

    int r = vbe_controller_info(&vib);
    assert(r == VBE_OK);

    for(int i = 0; mde[i] != 0xffff; i++) {
        struct vbemodeinfoblock mib;

        r = vbe_mode_info(mde[i], &mib);
        assert(r == VBE_OK);

        struct mode *m = &modes[nmodes++];

        m->xres = mib.xresolution;
        m->yres = mib.yresolution;
        m->bpp = mib.bitsperpixel;
        m->mode = mde[i];
    }

    // Initialize framebuffer service
    errval_t err = fb_export(NULL, listen_cb, connect_cb, get_default_waitset(),
                             IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    // Register our device driver
    errval_t err = pci_client_connect();
    assert(err_is_ok(err));

    err = pci_register_driver_noirq(vbe_init, PCI_CLASS_DISPLAY, PCI_DONT_CARE,
                                    PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                                    PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to register VBE driver");
        printf("vbe: failed to register driver\n");
        return 1;
    }

    messages_handler_loop();
}
