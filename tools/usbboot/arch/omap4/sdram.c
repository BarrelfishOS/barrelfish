/*
 * sdram.c
 *
 * Copyright(c) 2010 Texas Instruments.   All rights reserved.
 *
 * Texas Instruments, <www.ti.com>
 * Richard Woodruff <r-woodruff2@ti.com>
 * Santosh Shilimkar <santosh.shilimkar@ti.com>
 * Aneesh V	<aneesh@ti.com>
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *  * Neither the name Texas Instruments nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <aboot/aboot.h>
#include <aboot/io.h>
#include <omap4/hw.h>

#define CONFIG_OMAP4_SDC		1

#define MR0_ADDR			0
#define MR1_ADDR			1
#define MR2_ADDR			2
#define MR4_ADDR			4
#define MR10_ADDR			10
#define MR16_ADDR			16
#define REF_EN				0x40000000

/* defines for MR1 */
#define MR1_BL4				2
#define MR1_BL8				3
#define MR1_BL16			4

#define MR1_BT_SEQ			0
#define BT_INT				1

#define MR1_WC				0
#define MR1_NWC				1

#define MR1_NWR3			1
#define MR1_NWR4			2
#define MR1_NWR5			3
#define MR1_NWR6			4
#define MR1_NWR7			5
#define MR1_NWR8			6

#define MR1_VALUE (MR1_NWR3<<5) | (MR1_WC<<4) | (MR1_BT_SEQ<<3) | (MR1_BL8<<0)

/* defines for MR2 */
#define MR2_RL3_WL1			1
#define MR2_RL4_WL2			2
#define MR2_RL5_WL2			3
#define MR2_RL6_WL3			4

/* defines for MR10 */
#define MR10_ZQINIT			0xFF
#define MR10_ZQRESET			0xC3
#define MR10_ZQCL			0xAB
#define MR10_ZQCS			0x56


/* TODO: FREQ update method is not working so shadow registers programming
 * is just for same of completeness. This would be safer if auto
 * trasnitions are working
 */
#define FREQ_UPDATE_EMIF

/* EMIF Needs to be configured@19.2 MHz and shadow registers
 * should be programmed for new OPP.
 */

/* Elpida 2x2Gbit */
#define SDRAM_CONFIG_INIT		0x80800EB1
#define DDR_PHY_CTRL_1_INIT		0x849FFFF5
#define READ_IDLE_CTRL			0x000501FF
#define PWR_MGMT_CTRL			0x4000000f
#define PWR_MGMT_CTRL_OPP100		0x4000000f
#define ZQ_CONFIG			0x500b3215

#define CS1_MR(mr)	((mr) | 0x80000000)

void reset_phy(unsigned int base)
{
	writel(readl(base + IODFT_TLGC) | (1 << 10),
			base + IODFT_TLGC);
}

/* TODO: FREQ update method is not working so shadow registers programming
 * is just for same of completeness. This would be safer if auto
 * trasnitions are working
 */
static void emif_config(unsigned int base, const struct ddr_regs *ddr_regs)
{
	unsigned int reg_value;

	/*
	 * set SDRAM CONFIG register
	 * EMIF_SDRAM_CONFIG[31:29] REG_SDRAM_TYPE = 4 for LPDDR2-S4
	 * EMIF_SDRAM_CONFIG[28:27] REG_IBANK_POS = 0
	 * EMIF_SDRAM_CONFIG[13:10] REG_CL = 3
	 * EMIF_SDRAM_CONFIG[6:4] REG_IBANK = 3 - 8 banks
	 * EMIF_SDRAM_CONFIG[3] REG_EBANK = 0 - CS0
 	 * EMIF_SDRAM_CONFIG[2:0] REG_PAGESIZE = 2  - 512- 9 column
	 * JDEC specs - S4-2Gb --8 banks -- R0-R13, C0-c8
	 */
	writel(readl(base + EMIF_LPDDR2_NVM_CONFIG) & 0xBFFFFFFF,
			base + EMIF_LPDDR2_NVM_CONFIG);
	writel(ddr_regs->config_init, base + EMIF_SDRAM_CONFIG);

	/* PHY control values */
	writel(DDR_PHY_CTRL_1_INIT, base + EMIF_DDR_PHY_CTRL_1);
	writel(ddr_regs->phy_ctrl_1, base + EMIF_DDR_PHY_CTRL_1_SHDW);

	/*
	 * EMIF_READ_IDLE_CTRL
	 */
	writel(READ_IDLE_CTRL, base + EMIF_READ_IDLE_CTRL);
	writel(READ_IDLE_CTRL, base + EMIF_READ_IDLE_CTRL_SHDW);

	/*
	 * EMIF_SDRAM_TIM_1
	 */
	writel(ddr_regs->tim1, base + EMIF_SDRAM_TIM_1);
	writel(ddr_regs->tim1, base + EMIF_SDRAM_TIM_1_SHDW);

	/*
	 * EMIF_SDRAM_TIM_2
	 */
	writel(ddr_regs->tim2, base + EMIF_SDRAM_TIM_2);
	writel(ddr_regs->tim2, base + EMIF_SDRAM_TIM_2_SHDW);

	/*
	 * EMIF_SDRAM_TIM_3
	 */
	writel(ddr_regs->tim3, base + EMIF_SDRAM_TIM_3);
	writel(ddr_regs->tim3, base + EMIF_SDRAM_TIM_3_SHDW);

	writel(ddr_regs->zq_config, base + EMIF_ZQ_CONFIG);
	/*
	 * EMIF_PWR_MGMT_CTRL
	 */
	/*
	 * poll MR0 register (DAI bit)
	 * REG_CS[31] = 0 -- Mode register command to CS0
	 * REG_REFRESH_EN[30] = 1 -- Refresh enable after MRW
	 * REG_ADDRESS[7:0] = 00 -- Refresh enable after MRW
	 */

	writel(MR0_ADDR, base + EMIF_LPDDR2_MODE_REG_CFG);
	do {
		reg_value = readl(base + EMIF_LPDDR2_MODE_REG_DATA);
	} while ((reg_value & 0x1) != 0);

	writel(CS1_MR(MR0_ADDR), base + EMIF_LPDDR2_MODE_REG_CFG);
	do {
		reg_value = readl(base + EMIF_LPDDR2_MODE_REG_DATA);
	} while ((reg_value & 0x1) != 0);


	/* set MR10 register */
	writel(MR10_ADDR, base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(MR10_ZQINIT, base + EMIF_LPDDR2_MODE_REG_DATA);
	writel(CS1_MR(MR10_ADDR), base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(MR10_ZQINIT, base + EMIF_LPDDR2_MODE_REG_DATA);

	/* wait for tZQINIT=1us  */
	sdelay(10);

	/* set MR1 register */
	writel(MR1_ADDR, base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(ddr_regs->mr1, base + EMIF_LPDDR2_MODE_REG_DATA);
	writel(CS1_MR(MR1_ADDR), base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(ddr_regs->mr1, base + EMIF_LPDDR2_MODE_REG_DATA);


	/* set MR2 register RL=6 for OPP100 */
	writel(MR2_ADDR, base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(ddr_regs->mr2, base + EMIF_LPDDR2_MODE_REG_DATA);
	writel(CS1_MR(MR2_ADDR), base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(ddr_regs->mr2, base + EMIF_LPDDR2_MODE_REG_DATA);

	/* Set SDRAM CONFIG register again here with final RL-WL value */
	writel(ddr_regs->config_final, base + EMIF_SDRAM_CONFIG);
	writel(ddr_regs->phy_ctrl_1, base + EMIF_DDR_PHY_CTRL_1);

	/*
	 * EMIF_SDRAM_REF_CTRL
	 * refresh rate = DDR_CLK / reg_refresh_rate
	 * 3.9 uS = (400MHz)	/ reg_refresh_rate
	 */
	writel(ddr_regs->ref_ctrl, base + EMIF_SDRAM_REF_CTRL);
	writel(ddr_regs->ref_ctrl, base + EMIF_SDRAM_REF_CTRL_SHDW);

	/* set MR16 register */
	writel(MR16_ADDR | REF_EN, base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(0, base + EMIF_LPDDR2_MODE_REG_DATA);
	writel(CS1_MR(MR16_ADDR | REF_EN),
			base + EMIF_LPDDR2_MODE_REG_CFG);
	writel(0, base + EMIF_LPDDR2_MODE_REG_DATA);
	/* LPDDR2 init complete */

}
/*****************************************
 * Routine: ddr_init
 * Description: Configure DDR
 * EMIF1 -- CS0 -- DDR1 (256 MB)
 * EMIF2 -- CS0 -- DDR2 (256 MB)
 *****************************************/
void omap4_ddr_init(const struct ddr_regs *emif1_ddr_regs,
		    const struct ddr_regs *emif2_ddr_regs)
{
	/* DDR needs to be initialised @ 19.2 MHz
	 * So put core DPLL in bypass mode
	 * Configure the Core DPLL but don't lock it
	 */
	configure_core_dpll_no_lock();

	/* No IDLE: BUG in SDC */
	writel(0x0, EMIF1_BASE + EMIF_PWR_MGMT_CTRL);
	writel(0x0, EMIF2_BASE + EMIF_PWR_MGMT_CTRL);

	/* Configure EMIF1 */
	emif_config(EMIF1_BASE, emif1_ddr_regs);

	/* Configure EMIF2 */
	emif_config(EMIF2_BASE, emif2_ddr_regs);
	/* Lock Core using shadow CM_SHADOW_FREQ_CONFIG1 */
	lock_core_dpll_shadow();
	/* TODO: SDC needs few hacks to get DDR freq update working */

	/* Set DLL_OVERRIDE = 0 */
	writel(0x0, CM_DLL_CTRL);

	sdelay(200);

	/* Check for DDR PHY ready for EMIF1 & EMIF2 */
	while(((readl(EMIF1_BASE + EMIF_STATUS) & 0x04) != 0x04)
		|| ((readl(EMIF2_BASE + EMIF_STATUS) & 0x04) != 0x04));

	/* Reprogram the DDR PYHY Control register */
	/* PHY control values */

	sr32(CM_MEMIF_EMIF_1_CLKCTRL, 0, 32, 0x1);
        sr32(CM_MEMIF_EMIF_2_CLKCTRL, 0, 32, 0x1);

	/* Put the Core Subsystem PD to ON State */

	/* No IDLE: BUG in SDC */
	writel(0x80000000, EMIF1_BASE + EMIF_PWR_MGMT_CTRL);
	writel(0x80000000, EMIF2_BASE + EMIF_PWR_MGMT_CTRL);

	writel(0x0A300000, EMIF1_BASE + EMIF_L3_CONFIG);
	writel(0x0A300000, EMIF2_BASE + EMIF_L3_CONFIG);

	/*
	 * DMM : DMM_LISA_MAP_0(Section_0)
	 * [31:24] SYS_ADDR 		0x80
	 * [22:20] SYS_SIZE		0x7 - 2Gb
	 * [19:18] SDRC_INTLDMM		0x1 - 128 byte
	 * [17:16] SDRC_ADDRSPC 	0x0
	 * [9:8] SDRC_MAP 		0x3
	 * [7:0] SDRC_ADDR		0X0
	 */

	reset_phy(EMIF1_BASE);
	reset_phy(EMIF2_BASE);
}
