/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <arm_hal.h>

#include <omap44xx_cm2.h> // for turning on I2C clocks
#include <ti_i2c.h>
#include <ti_i2c_dev.h>

// there are 4 GP i2c controllers on the pandaboard
#define I2C_COUNT 4
static ti_i2c_t i2c[I2C_COUNT];
static bool i2c_initialized[I2C_COUNT];

static lpaddr_t i2c_pbase[I2C_COUNT] = 
{
    0x48070000u,
    0x48072000u,
    0x48060000u,
    0x48350000u,
};

// default timeout for waits in ticks
#define DEFAULT_TIMEOUT (tsc_get_hz() / 4)

#if 0
#define PBS (64*1024)
static char PRBUF[PBS];
#define PRBUFL PRBUF, (PBS-1)
#endif

/*
 * \brief initialize I2C controller `i`.
 */
void ti_i2c_init(int i) {
    // map & initialize mackerel device
    mackerel_addr_t i2c_vbase = omap_dev_map(i2c_pbase[i]);
    ti_i2c_initialize(&i2c[i], i2c_vbase);

    ti_i2c_t *dev = &i2c[i];

    // turn on clocks
    cm2_enable_i2c(i);

    // TODO?: enable interrupts

    // Disable i2c controller
    ti_i2c_con_wr(dev, 0x0000);

    // Issue soft reset
    ti_i2c_sysc_srst_wrf(dev, 0x1);

    // re-enable & check for reset done
    ti_i2c_con_en_wrf(dev, 0x1);

    while (ti_i2c_syss_rdone_rdf(dev) == 0x0) {
        // wait for reset done
    }

    // disable i2c controller again
    ti_i2c_con_wr(dev, 0x0);

    // Program prescaler to obtain ~12MHz internal clock
    // depends on the functional clock, I2Ci_FCLK is 96MHz, so the best
    // prescaler value is 0x7 as the divider is taken +1 (so setting 7
    // results in dividing by 8).
    ti_i2c_psc_wr(dev, 0x7);

    // set bitrate to 100kbps -- values taken from freebsd
    ti_i2c_scll_scl_wrf(dev, 0xd);
    ti_i2c_sclh_scl_wrf(dev, 0xf);

    // optional: configure HS mode

    // configure own address
    // according to freebsd this is not necessary in single master mode -SG

    // set rx & tx threshold -- set to 5 (register value + 1)
    ti_i2c_buf_txtrsh_wrf(dev, 0x4);
    ti_i2c_buf_rxtrsh_wrf(dev, 0x4);

    // bring controller out of reset
    //  Fast/Standard mode
    ti_i2c_con_opmode_wrf(dev, ti_i2c_opmode_fs);
    //  Enable
    ti_i2c_con_en_wrf(dev, 0x1);

    // Initialize the controller

    // configure master mode
    ti_i2c_con_mst_wrf(dev, 0x1);

    // enable interrupts for receive and transmit data ready
    ti_i2c_irqenable_clr_wr(dev, 0xffff);
    ti_i2c_irqenable_set_xrdy_ie_wrf(dev, 0x1);
    ti_i2c_irqenable_set_rrdy_ie_wrf(dev, 0x1);

    // enable DMA
    // we're not using DMA for now... -SG

    // set initialized flag
    i2c_initialized[i] = true;

    return;
}

static inline bool ti_i2c_poll_stat(ti_i2c_t *dev, ti_i2c_irqstatus_t flags,
        ti_i2c_irqstatus_t *retflags, int32_t timeout)
{
    // poll until timeout
    uint32_t start_ticks = tsc_read();
    uint32_t ticks;
    int32_t waittime = timeout;

    while (waittime > 0) {
        ti_i2c_irqstatus_t stat = ti_i2c_stat_rd(dev);
        if (stat & flags) {
            if (retflags) {
                *retflags = stat;
            }
            return true;
        }
        ticks = tsc_read();
        waittime -= (ticks - start_ticks);
        start_ticks = ticks;
    }
    return false;
}

static bool ti_i2c_wait_for_free_bus(ti_i2c_t *dev, int32_t timeout)
{
    // check if bus-busy == 0 --> bus free
    if (ti_i2c_stat_bb_rdf(dev) == 0) {
        return true;
    }

    // poll while bus busy or until timeout
    return ti_i2c_poll_stat(dev, ti_i2c_irq_flag_bb, NULL, timeout);
}

static errval_t
ti_i2c_read(ti_i2c_t *dev, uint8_t *buf, uint16_t length)
{
    bool wfb;
    wfb = ti_i2c_wait_for_free_bus(dev, DEFAULT_TIMEOUT);
    if (!wfb) {
        printk(LOG_ERR, "wait for bus free timed out\n");
        return SYS_ERR_I2C_WAIT_FOR_BUS;
    }

    // TODO: interrupt-driven?

    // write number of bytes to read
    ti_i2c_cnt_wr(dev, length);

    // clear write bit & initiate the read transaction, setting
    // the start bit (STT) starts the transaction
    ti_i2c_con_t con = ti_i2c_con_rd(dev);
    con = ti_i2c_con_trx_insert(con, 0);
    con = ti_i2c_con_mst_insert(con, 1);
    con = ti_i2c_con_stp_insert(con, 1);
    con = ti_i2c_con_stt_insert(con, 1);
    ti_i2c_con_wr(dev, con);

    ti_i2c_irqstatus_t events, retevents;

    events = ti_i2c_irq_flag_al // arbitration lost
           | ti_i2c_irq_flag_nack // no acknowledgement
           | ti_i2c_irq_flag_ardy // register access ready
           | ti_i2c_irq_flag_rdr // receive draining
           | ti_i2c_irq_flag_rrdy; // receive ready

    uint16_t amount = 0, sofar = 0;
    errval_t err = SYS_ERR_OK;
    // reading loop
    while (true) {
        // poll for NACK, AL, ARDY, RDR and RRDY
        while(!ti_i2c_poll_stat(dev, events, &retevents, DEFAULT_TIMEOUT)) {
            // poll for receive ready
        }

        if (retevents & ti_i2c_irq_flag_al) {
            printk(LOG_WARN, "arbitration lost\n");
            err = SYS_ERR_I2C_FAILURE;
            break;
        }

        if (retevents & ti_i2c_irq_flag_nack) {
            printk(LOG_WARN, "no ACK from slave\n");
            err = SYS_ERR_I2C_FAILURE;
            break;
        }

        // check if we have finished
        if (retevents & ti_i2c_irq_flag_ardy) {
            // register access ready --> transaction complete
            printk(LOG_NOTE, "ARDY transaction complete\n");
            err = SYS_ERR_OK;
            break;
        }

        // read some data
        if (retevents & ti_i2c_irq_flag_rdr) {
            // Receive draining interrupt --> we got the last data bytes
            printk(LOG_NOTE, "Receive draining interrupt\n");

            /* get the number of bytes in the FIFO */
            amount = ti_i2c_bufstat_rxstat_rdf(dev);
        }
        else if (retevents & ti_i2c_irq_flag_rrdy) {
            // Receive data ready interrupt --> got data
            printk(LOG_NOTE, "Receive data ready interrupt\n");

            // get the number of bytes in the FIFO
            amount = ti_i2c_bufstat_rxstat_rdf(dev);
            amount += 1;
        }

        // sanity check we haven't overwritten the array
        if ((sofar + amount) > length) {
            printk(LOG_WARN, "to many bytes to read\n");
            amount = (length - sofar);
        }

        // read the bytes from the fifo
        for (int i = 0; i < amount; i++) {
            buf[sofar++] = (uint8_t)ti_i2c_data_data_rdf(dev);
        }

        // attempt to clear the receive ready bits
        ti_i2c_stat_rdr_wrf(dev, 1);
        ti_i2c_stat_rrdy_wrf(dev, 1);
    }

    return err;
}

static errval_t
ti_i2c_write(ti_i2c_t *dev, uint8_t *buf, uint16_t length)
{
    bool wfb;
    wfb = ti_i2c_wait_for_free_bus(dev, DEFAULT_TIMEOUT);
    if (!wfb) {
        printk(LOG_ERR, "wait for bus free timed out\n");
        return SYS_ERR_I2C_WAIT_FOR_BUS;
    }

    // TODO: interrupt-driven?

    // write number of bytes to write
    ti_i2c_cnt_wr(dev, length);

    // set write bit & initiate the write transaction, setting
    // the start bit (STT) starts the transaction
    ti_i2c_con_t con = ti_i2c_con_rd(dev);
    con = ti_i2c_con_trx_insert(con, 1);
    con = ti_i2c_con_mst_insert(con, 1);
    con = ti_i2c_con_stp_insert(con, 1);
    con = ti_i2c_con_stt_insert(con, 1);
    ti_i2c_con_wr(dev, con);

    ti_i2c_irqstatus_t events, retevents;
    events = ti_i2c_irq_flag_al // arbitration lost
           | ti_i2c_irq_flag_nack // no acknowledgement
           | ti_i2c_irq_flag_ardy // register access ready
           | ti_i2c_irq_flag_xdr // transmit draining
           | ti_i2c_irq_flag_xrdy; // transmit ready

    uint16_t amount = 0, sofar = 0;
    errval_t err = SYS_ERR_OK;

    // writing loop
    while (true) {
        // poll for NACK, AL, ARDY, XDR and XRDY
        while(!ti_i2c_poll_stat(dev, events, &retevents, DEFAULT_TIMEOUT)) {
            // poll for events
        }

        if (retevents & ti_i2c_irq_flag_al) {
            printk(LOG_WARN, "arbitration lost\n");
            err = SYS_ERR_I2C_FAILURE;
            break;
        }

        if (retevents & ti_i2c_irq_flag_nack) {
            printk(LOG_WARN, "no ACK from slave\n");
            err = SYS_ERR_I2C_FAILURE;
            break;
        }

        // check if we have finished
        if (retevents & ti_i2c_irq_flag_ardy) {
            // register access ready --> transaction complete
            printk(LOG_NOTE, "ARDY transaction complete\n");
            err = SYS_ERR_OK;
            break;
        }

        // send some data
        if (retevents & ti_i2c_irq_flag_xdr) {
            // transmit draining interrupt --> we are sending the last data bytes
            printk(LOG_NOTE, "Receive draining interrupt\n");

            /* get the number of bytes that fit in the FIFO */
            amount = ti_i2c_bufstat_txstat_rdf(dev);
        }
        else if (retevents & ti_i2c_irq_flag_xrdy) {
            // transmit data ready interrupt --> can send data
            printk(LOG_NOTE, "Receive data ready interrupt\n");

            // get the number of bytes that fit in the FIFO
            amount = ti_i2c_bufstat_txstat_rdf(dev);
            amount += 1;
        }

        // sanity check so we don't write more bytes than available
        if ((sofar + amount) > length) {
            printk(LOG_WARN, "truncating length to not access data beyond buf\n");
            amount = (length - sofar);
        }

        // write the bytes to the fifo
        for (int i = 0; i < amount; i++) {
            ti_i2c_data_data_wrf(dev, buf[sofar++]);
        }

        // attempt to clear the receive ready bits
        ti_i2c_stat_xdr_wrf(dev, 1);
        ti_i2c_stat_xrdy_wrf(dev, 1);
    }

    // reset registers
    con = ti_i2c_con_default;
    con = ti_i2c_con_en_insert(con, 0x1);
    con = ti_i2c_con_mst_insert(con, 0x1);
    con = ti_i2c_con_stp_insert(con, 0x1);
    ti_i2c_con_wr(dev, con);

    return err;
}


/*
 * \brief Transfer (multiple) messages over I2C bus `i`
 */
errval_t ti_i2c_transfer(int devid, struct i2c_msg *msgs, size_t msgcount)
{
    if (!i2c_initialized[devid]) {
        return SYS_ERR_I2C_UNINITIALIZED;
    }
    ti_i2c_t *dev = &i2c[devid];
    uint16_t len;
    uint8_t *buf;

    for (int i = 0; i < msgcount; i++) {
        len = msgs[i].length;
        buf = msgs[i].buf;

        // zero length transfers are not allowed
        if (len == 0 || buf == NULL) {
            return SYS_ERR_I2C_ZERO_LENGTH_MSG;
        }

        // set slave address
        ti_i2c_sa_sa_wrf(dev, msgs[i].slave);

        // perform read or write
        if (msgs[i].flags & I2C_RD) {
            ti_i2c_read(dev, buf, len);
        } else {
            ti_i2c_write(dev, buf, len);
        }
    }

    return SYS_ERR_OK;
}

