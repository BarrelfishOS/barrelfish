/*
 * \brief Driver for face detection on OMAP 4460.
 *
 * \see OMAP TRM rev Z
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>


#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>

#include "fdif.h"

#include <dev/omap/omap44xx_cam_prm_dev.h>
#include <dev/omap/omap44xx_cam_cm2_dev.h>
#include <dev/omap/omap44xx_fdif_dev.h>

#define FDIF_IRQ (32+69)
#define PRINT_BUFFER_SIZE (1024*1024)

extern struct gimage lena_image;

struct fdif_driver_state {
    uint32_t level;
    omap44xx_cam_prm_t dev;
    omap44xx_fdif_t devfdif;
    omap44xx_cam_cm2_t devclk;
    char printbuf[PRINT_BUFFER_SIZE];
};

static void manage_clocks(struct fdif_driver_state* st, struct capref caps[])
{
    FDIF_DEBUG("Enable the clocks in domain CD_CAM\n");

    // Clock domain CAM
    lvaddr_t vbase;
    errval_t err;
    err = map_device_cap(caps[0], &vbase); // 0x4A009000, 4096
    assert(err_is_ok(err));

    omap44xx_cam_cm2_initialize(&st->devclk, (mackerel_addr_t)vbase);
    //omap44xx_cam_cm2_cm_cam_staticdep_l3_1_statdep_wrf(&st->devclk, 0x1);
    //omap44xx_cam_cm2_cm_cam_staticdep_memif_statdep_wrf(&st->devclk, 0x1);
    //omap44xx_cam_cm2_cm_cam_staticdep_ivahd_statdep_wrf(&st->devclk, 0x1);

    // Explicit enable && Force SW wakeup
    omap44xx_cam_cm2_cm_cam_fdif_clkctrl_modulemode_wrf(&st->devclk, 0x2);
    omap44xx_cam_cm2_cm_cam_clkstctrl_clktrctrl_wrf(&st->devclk, 0x2);
}

static void manage_power(struct fdif_driver_state* st, struct capref caps[])
{
    FDIF_DEBUG("Power-on the PD_CAM domain for fdif\n");

    // Power domain CAM
    lvaddr_t vbase;
    errval_t err;
    err = map_device_cap(caps[2], &vbase); // 0x4A307000, 4096
    assert(err_is_ok(err));

    omap44xx_cam_prm_initialize(&st->dev, (mackerel_addr_t)vbase);
    omap44xx_cam_prm_pm_cam_pwrstctrl_powerstate_wrf(&st->dev, 0x3);

    FDIF_DEBUG("%s:%d: Power OFF -> ON\n", __FUNCTION__, __LINE__);
    while (omap44xx_cam_prm_pm_cam_pwrstst_powerstatest_rdf(&st->dev)
            != 0x3);
    FDIF_DEBUG("%s:%d: Power is on.\n", __FUNCTION__, __LINE__);
}

static void read_result(struct fdif_driver_state* st)
{
    printf("Face detection completed:\n");
    printf("Read the results...\n");

    int faces = omap44xx_fdif_fd_dnum_dnum_rdf(&st->devfdif);
    printf("Faces found: %d\n", faces);
    //omap44xx_fdif_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->devfdif);
    //printf("%s\n", st->printbuf);

    for (int i = 0; i < faces; i++) {
        printf("Face %d:\n", i);
        int x = omap44xx_fdif_fd_centerx_centerx_rdf(&st->devfdif, i);
        int y = omap44xx_fdif_fd_centery_centery_rdf(&st->devfdif, i);
        printf("Position (X,Y): %d %d\n", x, y);

        int size = omap44xx_fdif_fd_confsize_size_rdf(&st->devfdif, i);
        int confidence = omap44xx_fdif_fd_confsize_conf_rdf(&st->devfdif, i);
        int angle = omap44xx_fdif_fd_angle_angle_rdf(&st->devfdif, i);
        printf("Size: %d Confidence: %d Angle: %d\n", size, confidence, angle);
    }
}

/*
 * \brief Interrupt handler for "finish" interrupt
 */
static void irq_handler(void *args)
{
    struct fdif_driver_state* st = (struct fdif_driver_state*) args;
    read_result(st);

    omap44xx_fdif_fdif_ctrl_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->devfdif);
    FDIF_DEBUG("%s\n", st->printbuf);

    omap44xx_cam_cm2_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->devclk);
    FDIF_DEBUG("%s\n", st->printbuf);

    omap44xx_cam_prm_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->dev);
    FDIF_DEBUG("%s\n", st->printbuf);


    omap44xx_fdif_fdif_irqstatus_finish_irq_wrf(&st->devfdif, 2, 1);

    // Go in Standby Mode
    // Again, module seems to go in standby just fine after its done
    // processing.
    //printf("%s:%d: go in standby\n", __FUNCTION__, __LINE__);
    //omap44xx_fdif_fdif_ctrl_mstandby_wrf(&st->devfdif, 0x1);
    //while(omap44xx_fdif_fdif_ctrl_mstandby_hdshk_rdf(&st->devfdif) != 0x0);

    // Disable Module Clocks
    omap44xx_cam_cm2_cm_cam_clkstctrl_clktrctrl_wrf(&st->devclk, 0x1);
    omap44xx_cam_cm2_cm_cam_fdif_clkctrl_modulemode_wrf(&st->devclk, 0x0);

    // Going Powermode ON-INACTIVE
    omap44xx_cam_prm_pm_cam_pwrstctrl_powerstate_wrf(&st->dev, 0x2);
    FDIF_DEBUG("%s:%d: Power ON -> ON-INACTIVE\n", __FUNCTION__, __LINE__);
    while (omap44xx_cam_prm_pm_cam_pwrstst_powerstatest_rdf(&st->dev)
            != 0x2);

    // Going Powermode ON-INACTIVE -> OFF
    // Must use lowpoerstatechange for that
    FDIF_DEBUG("%s:%d: Power ON-INACTIVE -> OFF\n", __FUNCTION__, __LINE__);
    omap44xx_cam_prm_pm_cam_pwrstctrl_powerstate_wrf(&st->dev, 0x0);
    omap44xx_cam_prm_pm_cam_pwrstctrl_lowpowerstatechange_wrf(&st->dev, 0x1);
    while (omap44xx_cam_prm_pm_cam_pwrstctrl_lowpowerstatechange_rdf(&st->dev)
            != 0x0);

    omap44xx_cam_cm2_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->devclk);
    FDIF_DEBUG("%s\n", st->printbuf);

    omap44xx_cam_prm_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->dev);
    FDIF_DEBUG("%s\n", st->printbuf);
}

/*
 * \brief Enable in poll-based mode
 *
 * \see OMAP TRM 9.4.1.2.1.1 Main Sequence – FDIF Polling Method
 */
static void __attribute__((__unused__)) enable_poll_mode(struct fdif_driver_state* st)
{
    omap44xx_fdif_fd_ctrl_run_wrf(&st->devfdif, 0x1);

    FDIF_DEBUG("%s:%d: Waiting until fdif is done...\n", __FUNCTION__, __LINE__);
    while (omap44xx_fdif_fd_ctrl_finish_rdf(&st->devfdif) != 0x1);

    read_result(st);
}


/*
 * \brief Enable in interrupt-based mode
 *
 * See OMAP TRM 9.4.1.2.1.2 Main Sequence – FDIF Interrupt Method
 */
static void enable_irq_mode(struct fdif_driver_state* st)
{
    errval_t err;

    omap44xx_fdif_fdif_irqenable_set_finish_irq_wrf(&st->devfdif, 2, 1);

    // Register interrupt handler in kernel
    err = inthandler_setup_arm(irq_handler, st, FDIF_IRQ);
    assert(err_is_ok(err));

    omap44xx_fdif_fd_ctrl_run_wrf(&st->devfdif, 0x1);
}

/*
 * \brief Set image parameters
 *
 * See OMAP TRM 9.4.1.2.1.3 Subsequence – Set Image Parameters
 */
static void set_image_params(struct fdif_driver_state* st, genpaddr_t picaddr, genpaddr_t wkaddr)
{
    // make sure 5 least significant bits are 0!
    omap44xx_fdif_fdif_picaddr_wr(&st->devfdif, picaddr);
    omap44xx_fdif_fdif_wkaddr_wr(&st->devfdif, wkaddr);

    omap44xx_fdif_fd_dcond_min_wrf(&st->devfdif, 0x0); // 40 pixel
    omap44xx_fdif_fd_dcond_dir_wrf(&st->devfdif, 0x0); // up?

    omap44xx_fdif_fd_startx_startx_wrf(&st->devfdif, 0x0);
    omap44xx_fdif_fd_starty_starty_wrf(&st->devfdif, 0x0);

    omap44xx_fdif_fd_sizex_sizex_wrf(&st->devfdif, 0x140); // TODO
    omap44xx_fdif_fd_sizey_sizey_wrf(&st->devfdif, 0xf0); // TODO
    omap44xx_fdif_fd_lhit_lhit_wrf(&st->devfdif, 0x5);
}

/**
 * Driver initialization function. This function is called by the driver domain
 * (see also 'create_handler' in ddomain_service.c).
 * Typically through a request from the device manager.
 *
 * The init function is supposed to set `dev` to the exported service iref.
 * The init function may use `bfi->dstate` to store additional state about the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \param[in]   name  The name of this driver instance.
 * \param[in]   flags Additional flags (The exact flags supported is device/driver specific).
 * \param[in]   c     Capabilities (for registers etc.) as provided by the device manager.
 *                    The exact layout of the `c` is device specific.
 * \param[out]  dev   The service iref over which the device can be contacted.
 *
 * \retval SYS_ERR_OK Device initialized successfully.
 * \retval LIB_ERR_MALLOC_FAIL Unable to allocate memory for the driver.
 */
static errval_t init(struct bfdriver_instance* bfi, const char* name, uint64_t flags,
                     struct capref* caps, size_t caps_len, char** args, size_t args_len, iref_t* dev) {
    FDIF_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    bfi->dstate = malloc(sizeof(struct fdif_driver_state));
    if (bfi->dstate == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    assert(bfi->dstate != NULL);

    struct fdif_driver_state* st = (struct fdif_driver_state*)bfi->dstate;

    // 1. Initialize the device:
    size_t img_size = 320 * 240 * 8; // 75 KB
    size_t working_size = img_size; // 51.25 KB is enough
    size_t retbytes;
    void *workarea;
    uint8_t *image;
    errval_t err;
    lpaddr_t vbase;

    // Face detect Module
    err = map_device_cap(caps[3], &vbase);
    assert(err_is_ok(err));
    omap44xx_fdif_initialize(&st->devfdif, (mackerel_addr_t)vbase);

    FDIF_DEBUG("FDIF Global Initialization\n");

    manage_clocks(st, caps);
    manage_power(st, caps);

    omap44xx_fdif_fdif_sysconfig_softreset_wrf(&st->devfdif, 1);
    while (omap44xx_fdif_fdif_sysconfig_softreset_rdf(&st->devfdif) != 0);

    omap44xx_fdif_fdif_sysconfig_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->devfdif);
    FDIF_DEBUG("%s\n", st->printbuf);

    omap44xx_fdif_fdif_sysconfig_idlemode_wrf(&st->devfdif, 0x2);
    omap44xx_fdif_fdif_sysconfig_standbymode_wrf(&st->devfdif, 0x2);

    omap44xx_fdif_fdif_sysconfig_pr(st->printbuf, PRINT_BUFFER_SIZE, &st->devfdif);
    FDIF_DEBUG("%s\n", st->printbuf);

    omap44xx_fdif_fdif_ctrl_max_tags_wrf(&st->devfdif, 0xA);

    struct capref img_cap;
    struct capref workarea_cap;

    err = frame_alloc(&img_cap, img_size, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes >= img_size);

    err = frame_alloc(&workarea_cap, working_size, &retbytes);
    assert(err_is_ok(err));
    assert(retbytes >= working_size);

    // Map space for image (as non-cacheable)
    err = vspace_map_one_frame_attr((void **)&image, img_size, img_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    assert(err_is_ok(err));
    err = vspace_map_one_frame(&workarea, working_size, workarea_cap,
                               NULL, NULL);
    assert(err_is_ok(err));

    FDIF_DEBUG("Image: width:%d height:%d bytes/pixel:%d\n",
               lena_image.width, lena_image.height, lena_image.bytes_per_pixel);
    for (int i = 0; i < lena_image.width * lena_image.height; i += 1) {
        image[i] = lena_image.pixel_data[i];
    }

    struct frame_identity ret;
    err = invoke_frame_identify(img_cap, &ret);
    assert (err_is_ok(err));

    struct frame_identity wkret;
    err = invoke_frame_identify(workarea_cap, &wkret);
    assert (err_is_ok(err));

    set_image_params(st, ret.base, wkret.base);

    // The following will set cm_cam_fdif_clkctrl_stdbysy to 0x0 (not in standby)
    // but it is apparently not needed (because we are in smart standby and
    // wake-up with magic as soon as we need it...?)
    //printf("%s:%d: // Wake up from standby\n", __FUNCTION__, __LINE__);
    //omap44xx_fdif_fdif_ctrl_mstandby_wrf(&st->devfdif, 0x0);
    //while(omap44xx_fdif_fdif_ctrl_mstandby_hdshk_rdf(&st->devfdif) != 0x1);
    enable_irq_mode(st);

    // 2. Export service to talk to the device:

    // 3. Set iref of your exported service (this is reported back to Kaluga)

    return SYS_ERR_OK;
}

/**
 * Instructs driver to attach to the device.
 * This function is only called if the driver has previously detached
 * from the device (see also detach).
 *
 * \note After detachment the driver can not assume anything about the
 * configuration of the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t attach(struct bfdriver_instance* bfi) {
    FDIF_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Instructs driver to detach from the device.
 * The driver must yield any control over to the device after this function returns.
 * The device may be left in any state.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t detach(struct bfdriver_instance* bfi) {
    FDIF_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Instructs the driver to go in a particular sleep state.
 * Supported states are platform/device specific.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    FDIF_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    struct fdif_driver_state* uds = bfi->dstate;
    uds->level = level;

    return SYS_ERR_OK;
}

/**
 * Destroys this driver instance.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t destroy(struct bfdriver_instance* bfi) {
    FDIF_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);
    struct fdif_driver_state* uds = bfi->dstate;
    free(uds);
    bfi->dstate = NULL;

    // XXX: Tear-down the service
    bfi->device = 0x0;

    return SYS_ERR_OK;
}

DEFINE_MODULE(fdif, init, attach, detach, set_sleep_level, destroy);
