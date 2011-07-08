/**
 * \file
 * \brief I/O APIC address space access functions implementation.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LPC_IOAPIC_IOAPIC_IMPL_H
#define LPC_IOAPIC_IOAPIC_IMPL_H

static inline uint32_t LPC_IOAPIC_ioapic_read_32(LPC_IOAPIC_t *dev,
                                                 size_t offset)
{
    // Select address via index register
    LPC_IOAPIC_ind_wr(dev, offset);

    // Return value from window register
    return LPC_IOAPIC_wdw_rd(dev);
}

static inline uint64_t LPC_IOAPIC_ioapic_read_64(LPC_IOAPIC_t *dev,
                                                 size_t offset)
{
    uint64_t ret;

    // Read LSW
    LPC_IOAPIC_ind_wr(dev, offset);
    ret = LPC_IOAPIC_wdw_rd(dev);

    // Read MSW
    LPC_IOAPIC_ind_wr(dev, offset + 1);
    ret |= (uint64_t)LPC_IOAPIC_wdw_rd(dev) << 32;

    return ret;
}

static inline void LPC_IOAPIC_ioapic_write_32(LPC_IOAPIC_t *dev, size_t offset,
                                              uint32_t value)
{
    // Select address via index register
    LPC_IOAPIC_ind_wr(dev, offset);

    // Write value to window register
    LPC_IOAPIC_wdw_wr(dev, value);
}

static inline void LPC_IOAPIC_ioapic_write_64(LPC_IOAPIC_t *dev, size_t offset,
                                              uint64_t value)
{
    // Write LSW
    LPC_IOAPIC_ind_wr(dev, offset);
    LPC_IOAPIC_wdw_wr(dev, value & 0xffffffff);

    // Write MSW
    LPC_IOAPIC_ind_wr(dev, offset + 1);
    LPC_IOAPIC_wdw_wr(dev, value >> 32);
}

#endif
