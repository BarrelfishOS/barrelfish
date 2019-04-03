/**
 * \file
 * \brief Physical address map for Cavium ThunderX CN88xx SoCs
 */

/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * See Cavium CN88xx Hardware Manual 2.55P, in particular Chapter 4
 */

#ifndef CN88XX_MAP_H
#define CN88XX_MAP_H

/*
 * DRAM/L2 cached memory
 */
#define CN88XX_MAP_LMC_OFFSET	0x0000000000000000UL
#define CN88XX_MAP_LMC_MASK	0x000000FFFFFFFFFFUL

/*
 * Converting a base address to a CCPI-node address
 */
#define CN88XX_MAP_LMC_CCPI_OFFSET(_ccpi,_a)			\
    (((_ccpi & 0x3) << 40) | ((_a) & CN88XX_MAP_LMC_MASK))
#define CN88XX_MAP_IO_CCPI_OFFSET(_ccpi,_a) \
    (((_ccpi & 0x3) << 44) | (_a))

/*
 * I/O spaces
 */
#define CN88XX_MAP_NCB_OFFSET   0x800000000000UL
#define CN88XX_MAP_RSL_OFFSET   0x87E000000000UL
#define CN88XX_MAP_AP_OFFSET    0x87F000000000UL
#define CN88XX_MAP_SLI_OFFSET   0x880000000000UL

/*
 * Devices
 */

/*
 * UARTs
 */
#define CN88XX_MAP_UART0_OFFSET 0x87E024000000UL
#define CN88XX_MAP_UART0_MSIX   0x87E024F00000UL
#define CN88XX_MAP_UART1_OFFSET 0x87E025000000UL
#define CN88XX_MAP_UART1_MSIX   0x87E025F00000UL

/*
 * The GIC
 */

#define CN88XX_MAP_GIC_GICD_OFFFSET	0x801000000000UL
#define CN88XX_MAP_GIC_CCS_OFFSET	0x801000010000UL
#define CN88XX_MAP_GIC_CCS_SIZE		0x000000010000UL
#define CN88XX_MAP_GIC_GITS_OFFSET	0x801000020000UL
#define CN88XX_MAP_GIC_GICRX_OFFSET	0x801080000000UL

#endif /* CN88XX_MAP_H */
