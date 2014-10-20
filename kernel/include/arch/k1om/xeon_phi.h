/**
 * \file
 * \brief Header for Xeon Phi Specific Addresses
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_H
#define XEON_PHI_H

#define XEON_PHI_CBOX_BASE           0x0ULL              /* P54C Core */
#define XEON_PHI_TXS0_BASE           0x0800780000ULL     /* Texture Sampler */
#define XEON_PHI_TXS1_BASE           0x0800770000ULL     /* Texture Sampler */
#define XEON_PHI_TXS2_BASE           0x0800760000ULL     /* Texture Sampler */
#define XEON_PHI_TXS3_BASE           0x0800750000ULL     /* Texture Sampler */
#define XEON_PHI_TXS4_BASE           0x0800740000ULL     /* Texture Sampler */
#define XEON_PHI_TXS5_BASE           0x0800730000ULL     /* Texture Sampler */
#define XEON_PHI_TXS6_BASE           0x0800720000ULL     /* Texture Sampler */
#define XEON_PHI_TXS7_BASE           0x0800710000ULL     /* Texture Sampler */
#define XEON_PHI_DBOX0_BASE          0x08007C0000ULL     /* Display Box Registers */
#define XEON_PHI_DBOX1_BASE          0x0800620000ULL     /* Display Box Registers */
#define XEON_PHI_TD0_BASE            0x08007C0000ULL     /* Dbox Tag Directory TD */
#define XEON_PHI_TD1_BASE            0x0800620000ULL     /* Dbox Tag Directory TD */
#define XEON_PHI_VBOX_BASE           0x08007B0000ULL     /* Video Box Registers */
#define XEON_PHI_SBOX_BASE           0x08007D0000ULL     /* PCIE Box Registers */
#define XEON_PHI_GBOX0_BASE          0x08007A0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX1_BASE          0x0800790000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX2_BASE          0x0800700000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX3_BASE          0x08006F0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX4_BASE          0x08006D0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX5_BASE          0x08006C0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX6_BASE          0x08006B0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_GBOX7_BASE          0x08006A0000ULL     /* Gbox Front Box Registers */
#define XEON_PHI_REUT0_BASE          0x08007A0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT1_BASE          0x0800790000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT2_BASE          0x0800700000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT3_BASE          0x08006F0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT4_BASE          0x08006D0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT5_BASE          0x08006C0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT6_BASE          0x08006B0000ULL     /* Gbox REUT interface Registers */
#define XEON_PHI_REUT7_BASE          0x08006A0000ULL     /* Gbox REUT interface Registers */

#define XEON_PHI_GBOX_CHANNEL0_BASE  0x0
#define XEON_PHI_GBOX_CHANNEL1_BASE  0x800
#define XEON_PHI_GBOX_CHANNEL2_BASE  0x800
#define XEON_PHI_GBOX_CHANNEL3_BASE  0x1000


#define XEON_PHI_SBOX_SIZE (64*1024)
#define XEON_PHI_SBOX_SIZE_BITS (16)

// TODO: Verify these...
#define XEON_PHI_GBOX_SIZE (64*1024)
#define XEON_PHI_DBOX_SIZE (64*1024)
#define XEON_PHI_CBOX_SIZE (64*1024)

#define XEON_PHI_SYSMEM_BASE 0x8000000000ULL
#define XEON_PHI_SYSMEM_SIZE_BITS 39
#define XEON_PHI_SYSMEM_SIZE (1ULL << XEON_PHI_SYSMEM_SIZE_BITS)



void xeon_phi_init_early(void);

#endif /* XEON_PHI_H*/
