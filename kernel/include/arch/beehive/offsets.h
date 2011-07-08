/**
 * \file
 * \brief Beehive address space sizes and offsets
 *
 * The Beehive has no translation logic, and an address space layout
 * which is different on the instruction side and the data side.
 * Barrelfish memory management system currently doesnt deal with this
 * so instead we just pretend we have a simple 32-bit address space.
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OFFSETS_H
#define OFFSETS_H

#ifndef __ASSEMBLER__

/*
 * This must be defined for kernel/capabilities.c.  Maximum physical
 * address space mappable by the kernel.  We pretend 2GB since its not
 * clear whether it deals correctly with 4GB.
 */

#define PADDR_SPACE_LIMIT ((lpaddr_t)1 << 31)


/**
 * Takes absolute physical address addr and returns corresponding
 * kernel-space address.
 *
 * \param addr  Absolute physical address
 *
 * \return Corresponding kernel-space address.
 */
static inline lvaddr_t local_phys_to_mem(lpaddr_t addr)
{
  return (lvaddr_t)addr;
}

/**
 * Takes kernel-space address addr and returns corresponding physical
 * address.
 *
 * \param addr  Kernel-space address
 *
 * \return Corresponding physical address.
 */
static inline lpaddr_t mem_to_local_phys(lvaddr_t addr)
{
  return (lpaddr_t)addr;
}

/*
 * TODO XXX This is platform specific concept not architecture
 * specific
 */
static inline lpaddr_t gen_phys_to_local_phys(genpaddr_t addr)
{
    return (lpaddr_t)addr;
}

/*
 * TODO XXX This is platform specific concept not architecture
 * specific
 */
static inline genpaddr_t local_phys_to_gen_phys(lpaddr_t addr)
{
    return (genpaddr_t)addr;
}

#endif  // __ASSEMBLER__

#endif  // OFFSETS_H
