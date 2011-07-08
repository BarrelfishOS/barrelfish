/**
 * \file
 * \brief Classic 8259A PIC driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <pic.h>
#include <lpc_pic_dev.h>

/// The dual PIC
static LPC_PIC_t pic;

/**
 * \brief Send end of interrupt.
 */
void pic_eoi(int irq)
{
    // Send specific end of interrupt message
    static LPC_PIC_ocw2_t eoi = {
        .rsleoi = LPC_PIC_seoi
    };

    if(irq < 8) {
        eoi.level = irq;
        LPC_PIC_master_ocw2_wr(&pic, eoi);
    } else {
        eoi.level = irq - 8;
        LPC_PIC_slave_ocw2_wr(&pic, eoi);
    }
}

/**
 * \brief returns true iff the PIC has an interrupt pending
 */
bool pic_have_interrupt(int irq)
{
    static const LPC_PIC_ocw3_t read_is = {
        .rrc = LPC_PIC_read_is
    };

    if(irq < 8) {
        // send read ISR command
        LPC_PIC_master_ocw3_wr(&pic, read_is);
        // read ISR and check bit
        return (LPC_PIC_master_ocw3rd_rd(&pic) & (1 << irq)) != 0;
    } else {
        LPC_PIC_slave_ocw3_wr(&pic, read_is);
        return (LPC_PIC_slave_ocw3rd_rd(&pic) & (1 << (irq -8))) != 0;
    }
}

static int mask_to_interrupt(uint8_t mask)
{
    for (int i = 0; i < 8; i++) {
        if (mask & (1 << i)) {
            return i;
        }
    }
    return -1;
}

/**
 * \brief Queries the PIC for pending interrupts
 *
 * \returns IRQ number of pending interrupt, or -1 if nothing is pending
 */
int pic_pending_interrupt(void)
{
    static const LPC_PIC_ocw3_t read_is = {
        .rrc = LPC_PIC_read_is
    };

    uint8_t isr;

    // try master first
    LPC_PIC_master_ocw3_wr(&pic, read_is);
    isr = LPC_PIC_master_ocw3rd_rd(&pic);
    if (isr != 0) {
        return mask_to_interrupt(isr);
    }

    // try slave
    LPC_PIC_slave_ocw3_wr(&pic, read_is);
    isr = LPC_PIC_slave_ocw3rd_rd(&pic);
    if (isr != 0) {
        return mask_to_interrupt(isr) + 8;
    }

    return -1;
}

/**
 * \brief Initialize 8259A.
 *
 * Initializes both master and slave 8259A in the standard cascaded
 * way (slave attached to IR line 2 of master). Sets off interrupts by
 * 32, leaving the lower 32 IRQs reserved for processor exceptions, as
 * required by protected mode. Sets all interrupts to edge
 * triggered. Finally, masks out all interrupts. If an interrupt is
 * expected by the OS, it has to be unmasked individually.
 */
void pic_init(void)
{
    // setup mackerel state
    LPC_PIC_initialize(&pic, 0);

    LPC_PIC_icw1_t icw1 = { .ltim = 0 };
    LPC_PIC_pic_master_icw3_t master_icw3 = {
        .cascade = 1    /* Slaves attached to IR line 2 */
    };
    LPC_PIC_pic_slave_icw3_t slave_icw3 = {
        .slave_id = 2   /* This slave in IR line 2 of master */
    };
    LPC_PIC_icw4_t icw4 = {
        .aeoi = 0,
        .sfnm = 0
    };

    // Setup 8259A PIC for proper protected mode interrupt delivery
    /* ICW1 */
    LPC_PIC_master_icw1_wr(&pic, icw1);
    LPC_PIC_slave_icw1_wr(&pic, icw1);

    /* ICW2 */
    LPC_PIC_master_icw2_wr_raw(&pic, 0x20); // IDT offset 0x20
    LPC_PIC_slave_icw2_wr_raw(&pic, 0x28);  // IDT offset 0x28

    /* ICW3 */
    LPC_PIC_master_icw3_wr(&pic, master_icw3);
    LPC_PIC_slave_icw3_wr(&pic, slave_icw3);

    /* ICW4 */
    LPC_PIC_master_icw4_wr(&pic, icw4);
    LPC_PIC_slave_icw4_wr(&pic, icw4);

    if (CPU_IS_M5_SIMULATOR) {
        printf("Warning: not setting elcr1 elcr2 on M5\n");
    } else {
        // Set all interrupts to be edge triggered
        LPC_PIC_pic_master_trigger_t elcr1 = {
            .irq3_ecl = 0,
            .irq4_ecl = 0,
            .irq5_ecl = 0,
            .irq6_ecl = 0,
            .irq7_ecl = 0
        };
        LPC_PIC_master_trigger_wr(&pic, elcr1);

        LPC_PIC_pic_slave_trigger_t elcr2 = {
            .irq9_ecl = 0,
            .irq10_ecl = 0,
            .irq11_ecl = 0,
            .irq12_ecl = 0,
            .irq14_ecl = 0,
            .irq15_ecl = 0
        };
        LPC_PIC_slave_trigger_wr(&pic, elcr2);
    }

    // Mask all interrupts (except cascade IRQ 2)
    LPC_PIC_slave_ocw1_wr(&pic, 0xff);
    LPC_PIC_master_ocw1_wr(&pic, ~(1 << 2));
}

/**
 * \brief Enable/Disable interrupt 'irq'.
 *
 * Be careful to serialize calls to this function on a
 * multiprocessor. In general, the classic 8259A should not be used on
 * a multiprocessor.
 */
void pic_toggle_irq(int irq, bool enable)
{
    assert(irq >= 0 && irq <= 15);

    if(irq < 8) {
        // Master controller
        uint8_t mask = 1 << irq;
        uint8_t val = LPC_PIC_master_ocw1_rd(&pic);

        if(enable) {
            val &= ~mask;
        } else {
            val |= mask;
        }

        LPC_PIC_master_ocw1_wr(&pic, val);
    } else {
        // Slave controller
        uint8_t mask = 1 << (irq - 8);
        uint8_t val = LPC_PIC_slave_ocw1_rd(&pic);

        if(enable) {
            val &= ~mask;
        } else {
            val |= mask;
        }

        LPC_PIC_slave_ocw1_wr(&pic, val);
    }
}
