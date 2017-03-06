/******************************************************************************
*
* Copyright (C) 2010 - 2015 Xilinx, Inc.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* Use of the Software is limited solely to applications:
* (a) running on a Xilinx device, or
* (b) that interact with a Xilinx device through a bus or interconnect.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
* XILINX  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
* WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
* OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
* Except as contained in this notice, the name of the Xilinx shall not be used
* in advertising or otherwise to promote the sale, use or other dealings in
* this Software without prior written authorization from Xilinx.
*
******************************************************************************
*
* Copyright (c) 2016, ETH Zurich.
* All rights reserved.
*
* This file is distributed under the terms in the attached LICENSE file.
* If you do not find this file, copies can be found by writing to:
* ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
*
*****************************************************************************
*
* @file zynq7000_map.h
*
* This file contains the address definitions for the hard peripherals
* attached to the ARM Cortex A9 core in the Zynq7000-series SoCs.
*
* Derived from lib/bsp/standalone/src/cortexa9/xparameters_ps.h in the Xilinx
* 'embeddedsw' package.
*
******************************************************************************/

#ifndef _ZYNQ7000_MAP_H_
#define _ZYNQ7000_MAP_H_

#ifdef __cplusplus
extern "C" {
#endif

/************************** Constant Definitions *****************************/

/*
 * This block contains constant declarations for the peripherals
 * within the hardblock
 */

/* Canonical definitions for DDR MEMORY */
#define ZYNQ7_DDR_MEM_BASEADDR            0x00000000U
#define ZYNQ7_DDR_MEM_HIGHADDR            0x3FFFFFFFU

/* Canonical definitions for SLCR */
#define ZINQ7_XSLCR_NUM_INSTANCES         1U
#define ZINQ7_XSLCR_0_DEVICE_ID           0U
#define ZINQ7_XSLCR_0_BASEADDR            ZINQ7_SYS_CTRL_BASEADDR

/* Canonical definitions for SCU GIC */
#define ZINQ7_SCUGIC_NUM_INSTANCES        1U
#define ZINQ7_SCUGIC_SINGLE_DEVICE_ID     0U
#define ZINQ7_SCUGIC_CPU_BASEADDR         (ZINQ7_SCU_PERIPH_BASE + 0x00000100U)
#define ZINQ7_SCUGIC_DIST_BASEADDR        (ZINQ7_SCU_PERIPH_BASE + 0x00001000U)
#define ZINQ7_SCUGIC_ACK_BEFORE           0U

/* Canonical definitions for Global Timer */
#define ZINQ7_GLOBAL_TMR_NUM_INSTANCES    1U
#define ZINQ7_GLOBAL_TMR_DEVICE_ID        0U
#define ZINQ7_GLOBAL_TMR_BASEADDR         (ZINQ7_SCU_PERIPH_BASE + 0x00000200U)
#define ZINQ7_GLOBAL_TMR_INTR             ZINQ7_GLOBAL_TMR_INT_ID

/*
 * This block contains constant declarations for the peripherals
 * within the hardblock. These have been put for bacwards compatibilty
 */

#define ZINQ7_PERIPHERAL_BASEADDR         0xE0000000U
#define ZINQ7_UART0_BASEADDR              0xE0000000U
#define ZINQ7_UART1_BASEADDR              0xE0001000U
#define ZINQ7_USB0_BASEADDR               0xE0002000U
#define ZINQ7_USB1_BASEADDR               0xE0003000U
#define ZINQ7_I2C0_BASEADDR               0xE0004000U
#define ZINQ7_I2C1_BASEADDR               0xE0005000U
#define ZINQ7_SPI0_BASEADDR               0xE0006000U
#define ZINQ7_SPI1_BASEADDR               0xE0007000U
#define ZINQ7_CAN0_BASEADDR               0xE0008000U
#define ZINQ7_CAN1_BASEADDR               0xE0009000U
#define ZINQ7_GPIO_BASEADDR               0xE000A000U
#define ZINQ7_GEM0_BASEADDR               0xE000B000U
#define ZINQ7_GEM1_BASEADDR               0xE000C000U
#define ZINQ7_QSPI_BASEADDR               0xE000D000U
#define ZINQ7_PARPORT_CRTL_BASEADDR       0xE000E000U
#define ZINQ7_SDIO0_BASEADDR              0xE0100000U
#define ZINQ7_SDIO1_BASEADDR              0xE0101000U
#define ZINQ7_IOU_BUS_CFG_BASEADDR        0xE0200000U
#define ZINQ7_NAND_BASEADDR               0xE1000000U
#define ZINQ7_PARPORT0_BASEADDR           0xE2000000U
#define ZINQ7_PARPORT1_BASEADDR           0xE4000000U
#define ZINQ7_QSPI_LINEAR_BASEADDR        0xFC000000U
#define ZINQ7_SYS_CTRL_BASEADDR           0xF8000000U     /* AKA SLCR */
#define ZINQ7_TTC0_BASEADDR               0xF8001000U
#define ZINQ7_TTC1_BASEADDR               0xF8002000U
#define ZINQ7_DMAC0_SEC_BASEADDR          0xF8003000U
#define ZINQ7_DMAC0_NON_SEC_BASEADDR      0xF8004000U
#define ZINQ7_WDT_BASEADDR                0xF8005000U
#define ZINQ7_DDR_CTRL_BASEADDR           0xF8006000U
#define ZINQ7_DEV_CFG_APB_BASEADDR        0xF8007000U
#define ZINQ7_AFI0_BASEADDR               0xF8008000U
#define ZINQ7_AFI1_BASEADDR               0xF8009000U
#define ZINQ7_AFI2_BASEADDR               0xF800A000U
#define ZINQ7_AFI3_BASEADDR               0xF800B000U
#define ZINQ7_OCM_BASEADDR                0xF800C000U
#define ZINQ7_EFUSE_BASEADDR              0xF800D000U
#define ZINQ7_CORESIGHT_BASEADDR          0xF8800000U
#define ZINQ7_TOP_BUS_CFG_BASEADDR        0xF8900000U
#define ZINQ7_SCU_PERIPH_BASE             0xF8F00000U
#define ZINQ7_L2CC_BASEADDR               0xF8F02000U
#define ZINQ7_SAM_RAM_BASEADDR            0xFFFC0000U
#define ZINQ7_FPGA_AXI_S0_BASEADDR        0x40000000U
#define ZINQ7_FPGA_AXI_S1_BASEADDR        0x80000000U
#define ZINQ7_IOU_S_SWITCH_BASEADDR       0xE0000000U
#define ZINQ7_PERIPH_APB_BASEADDR         0xF8000000U

/* Shared Peripheral Interrupts (SPI) */
#define ZINQ7_CORE_PARITY0_INT_ID         32U
#define ZINQ7_CORE_PARITY1_INT_ID         33U
#define ZINQ7_L2CC_INT_ID                 34U
#define ZINQ7_OCMINTR_INT_ID              35U
#define ZINQ7_ECC_INT_ID                  36U
#define ZINQ7_PMU0_INT_ID                 37U
#define ZINQ7_PMU1_INT_ID                 38U
#define ZINQ7_SYSMON_INT_ID               39U
#define ZINQ7_DVC_INT_ID                  40U
#define ZINQ7_WDT_INT_ID                  41U
#define ZINQ7_TTC0_0_INT_ID               42U
#define ZINQ7_TTC0_1_INT_ID               43U
#define ZINQ7_TTC0_2_INT_ID               44U
#define ZINQ7_DMA0_ABORT_INT_ID           45U
#define ZINQ7_DMA0_INT_ID                 46U
#define ZINQ7_DMA1_INT_ID                 47U
#define ZINQ7_DMA2_INT_ID                 48U
#define ZINQ7_DMA3_INT_ID                 49U
#define ZINQ7_SMC_INT_ID                  50U
#define ZINQ7_QSPI_INT_ID                 51U
#define ZINQ7_GPIO_INT_ID                 52U
#define ZINQ7_USB0_INT_ID                 53U
#define ZINQ7_GEM0_INT_ID                 54U
#define ZINQ7_GEM0_WAKE_INT_ID            55U
#define ZINQ7_SDIO0_INT_ID                56U
#define ZINQ7_I2C0_INT_ID                 57U
#define ZINQ7_SPI0_INT_ID                 58U
#define ZINQ7_UART0_INT_ID                59U
#define ZINQ7_CAN0_INT_ID                 60U
#define ZINQ7_FPGA0_INT_ID                61U
#define ZINQ7_FPGA1_INT_ID                62U
#define ZINQ7_FPGA2_INT_ID                63U
#define ZINQ7_FPGA3_INT_ID                64U
#define ZINQ7_FPGA4_INT_ID                65U
#define ZINQ7_FPGA5_INT_ID                66U
#define ZINQ7_FPGA6_INT_ID                67U
#define ZINQ7_FPGA7_INT_ID                68U
#define ZINQ7_TTC1_0_INT_ID               69U
#define ZINQ7_TTC1_1_INT_ID               70U
#define ZINQ7_TTC1_2_INT_ID               71U
#define ZINQ7_DMA4_INT_ID                 72U
#define ZINQ7_DMA5_INT_ID                 73U
#define ZINQ7_DMA6_INT_ID                 74U
#define ZINQ7_DMA7_INT_ID                 75U
#define ZINQ7_USB1_INT_ID                 76U
#define ZINQ7_GEM1_INT_ID                 77U
#define ZINQ7_GEM1_WAKE_INT_ID            78U
#define ZINQ7_SDIO1_INT_ID                79U
#define ZINQ7_I2C1_INT_ID                 80U
#define ZINQ7_SPI1_INT_ID                 81U
#define ZINQ7_UART1_INT_ID                82U
#define ZINQ7_CAN1_INT_ID                 83U
#define ZINQ7_FPGA8_INT_ID                84U
#define ZINQ7_FPGA9_INT_ID                85U
#define ZINQ7_FPGA10_INT_ID               86U
#define ZINQ7_FPGA11_INT_ID               87U
#define ZINQ7_FPGA12_INT_ID               88U
#define ZINQ7_FPGA13_INT_ID               89U
#define ZINQ7_FPGA14_INT_ID               90U
#define ZINQ7_FPGA15_INT_ID               91U

/* Private Peripheral Interrupts (PPI) */
#define ZINQ7_GLOBAL_TMR_INT_ID           27U /* SCU Global Timer interrupt */
#define ZINQ7_FIQ_INT_ID                  28U /* FIQ from FPGA fabric */
#define ZINQ7_SCU_TMR_INT_ID              29U /* SCU Private Timer interrupt */
#define ZINQ7_SCU_WDT_INT_ID              30U /* SCU Private WDT interrupt */
#define ZINQ7_IRQ_INT_ID                  31U /* IRQ from FPGA fabric */

#define ZINQ7_SCUTIMER_DEVICE_ID          0U
#define ZINQ7_SCUWDT_DEVICE_ID            0U

#ifdef __cplusplus
}
#endif

#endif /* _ZYNQ7000_MAP_H_ */
