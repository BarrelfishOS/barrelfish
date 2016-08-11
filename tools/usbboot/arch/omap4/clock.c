/*
 * clock.c
 *
 * Copyright(c) 2010 Texas Instruments.   All rights reserved.
 *
 * Texas Instruments, <www.ti.com>
 * Richard Woodruff <r-woodruff2@ti.com>
 * Rajendra Nayak <rnayak@ti.com>
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


//#include <omap4/clocks.h>

#define LDELAY      12000000

#define PLL_STOP		1 /* PER & IVA */
#define PLL_MN_POWER_BYPASS	4
#define PLL_LOW_POWER_BYPASS	5 /* MPU, IVA & CORE */
#define PLL_FAST_RELOCK_BYPASS	6 /* CORE */
#define PLL_LOCK		7 /* MPU, IVA, CORE & PER */

#define BIT0 (1<<0)
#define BIT16 (1<<16)
#define BIT17 (1<<17)
#define BIT18 (1<<18)

#define CONFIG_OMAP4_SDC 1

/* clk sel is 12M / 13M / 16.8M / 19.2M / 26M / 27M / 38.4M */
/* we only support 38.4M here */

/* #define CONFIG_MPU_1000 1 */
/* #define CONFIG_MPU_800 1 */
#define CONFIG_MPU_600 1

struct dpll_param mpu_dpll_param = {
#ifdef CONFIG_MPU_600
	0x7d, 0x07, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
#elif CONFIG_MPU_800 /* 796.8MHz */
	0xa6, 0x07, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
#elif CONFIG_MPU_1000
	0x69, 0x03, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
#else /* 332.8MHz */
	0x1a, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,	
#endif
};

const struct dpll_param per_dpll_param = {
	0x14, 0x00, 0x08, 0x06, 0x0c, 0x09, 0x04, 0x05,
};

struct dpll_param iva_dpll_param = {
#ifdef CONFIG_OMAP4_SDC
	0x61, 0x03, 0x00, 0x00, 0x04, 0x07, 0x00, 0x00,
#else
	0x61, 0x03, 0x00, 0x00, 0x04, 0x07, 0x00, 0x00,
#endif
};

struct dpll_param core_dpll_param_ddr400mhz = {
	0x7d, 0x05, 0x01, 0x05, 0x08, 0x04, 0x06, 0x05,
};

struct dpll_param abe_dpll_param = {
#ifdef CONFIG_OMAP4_SDC
	0x40, 0x18, 0x1, 0x1, 0x0, 0x0, 0x0, 0x0,
#else
	0x40, 0x18, 0x1, 0x1, 0x0, 0x0, 0x0, 0x0,
#endif
};

struct dpll_param usb_dpll_param = {
#ifdef CONFIG_OMAP4_SDC
	0x32, 0x1, 0x2, 0x0, 0x0, 0x0, 0x0, 0x0,
#else
	0x32, 0x1, 0x2, 0x0, 0x0, 0x0, 0x0, 0x0,
#endif
};

typedef struct dpll_param dpll_param;

static void configure_mpu_dpll(dpll_param *dpll_param_p)
{
	/* Unlock the MPU dpll */
	sr32(CM_CLKMODE_DPLL_MPU, 0, 3, PLL_MN_POWER_BYPASS);
	wait_on_value(BIT0, 0, CM_IDLEST_DPLL_MPU, LDELAY);

	/* Disable DPLL autoidle */
	sr32(CM_AUTOIDLE_DPLL_MPU, 0, 3, 0x0);

	/* Set M,N,M2 values */
	sr32(CM_CLKSEL_DPLL_MPU, 8, 11, dpll_param_p->m);
	sr32(CM_CLKSEL_DPLL_MPU, 0, 6, dpll_param_p->n);
	sr32(CM_DIV_M2_DPLL_MPU, 0, 5, dpll_param_p->m2);
	sr32(CM_DIV_M2_DPLL_MPU, 8, 1, 0x1);

	/* Lock the mpu dpll */
	sr32(CM_CLKMODE_DPLL_MPU, 0, 3, PLL_LOCK | 0x10);
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_MPU, LDELAY);
}

static void configure_iva_dpll(dpll_param *dpll_param_p)
{
	/* Unlock the IVA dpll */
	sr32(CM_CLKMODE_DPLL_IVA, 0, 3, PLL_MN_POWER_BYPASS);
	wait_on_value(BIT0, 0, CM_IDLEST_DPLL_IVA, LDELAY);

	/* CM_BYPCLK_DPLL_IVA = CORE_X2_CLK/2 */
	sr32(CM_BYPCLK_DPLL_IVA, 0, 2, 0x1);

	/* Disable DPLL autoidle */
	sr32(CM_AUTOIDLE_DPLL_IVA, 0, 3, 0x0);

	/* Set M,N,M4,M5 */
	sr32(CM_CLKSEL_DPLL_IVA, 8, 11, dpll_param_p->m);
	sr32(CM_CLKSEL_DPLL_IVA, 0, 7, dpll_param_p->n);
	sr32(CM_DIV_M4_DPLL_IVA, 0, 5, dpll_param_p->m4);
	sr32(CM_DIV_M4_DPLL_IVA, 8, 1, 0x1);
	sr32(CM_DIV_M5_DPLL_IVA, 0, 5, dpll_param_p->m5);
	sr32(CM_DIV_M5_DPLL_IVA, 8, 1, 0x1);

	/* Lock the iva dpll */
	sr32(CM_CLKMODE_DPLL_IVA, 0, 3, PLL_LOCK);
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_IVA, LDELAY);
}

static void configure_per_dpll(const dpll_param *dpll_param_p)
{
	/* Unlock the PER dpll */
	sr32(CM_CLKMODE_DPLL_PER, 0, 3, PLL_MN_POWER_BYPASS);
	wait_on_value(BIT0, 0, CM_IDLEST_DPLL_PER, LDELAY);

	/* Disable autoidle */
	sr32(CM_AUTOIDLE_DPLL_PER, 0, 3, 0x0);

	sr32(CM_CLKSEL_DPLL_PER, 8, 11, dpll_param_p->m);
	sr32(CM_CLKSEL_DPLL_PER, 0, 6, dpll_param_p->n);
	sr32(CM_DIV_M2_DPLL_PER, 0, 5, dpll_param_p->m2);
	sr32(CM_DIV_M2_DPLL_PER, 8, 1, 0x1);
	sr32(CM_DIV_M3_DPLL_PER, 0, 5, dpll_param_p->m3);
	sr32(CM_DIV_M3_DPLL_PER, 8, 1, 0x1);
	sr32(CM_DIV_M4_DPLL_PER, 0, 5, dpll_param_p->m4);
	sr32(CM_DIV_M4_DPLL_PER, 8, 1, 0x1);
	sr32(CM_DIV_M5_DPLL_PER, 0, 5, dpll_param_p->m5);
	sr32(CM_DIV_M5_DPLL_PER, 8, 1, 0x1);
	sr32(CM_DIV_M6_DPLL_PER, 0, 5, dpll_param_p->m6);
	sr32(CM_DIV_M6_DPLL_PER, 8, 1, 0x1);
	sr32(CM_DIV_M7_DPLL_PER, 0, 5, dpll_param_p->m7);
	sr32(CM_DIV_M7_DPLL_PER, 8, 1, 0x1);

	/* Lock the per dpll */
	sr32(CM_CLKMODE_DPLL_PER, 0, 3, PLL_LOCK);
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_PER, LDELAY);
}

static void configure_abe_dpll(dpll_param *dpll_param_p)
{
	/* Select sys_clk as ref clk for ABE dpll */
	sr32(CM_ABE_PLL_REF_CLKSEL, 0, 32, 0x0);

	/* Unlock the ABE dpll */
	sr32(CM_CLKMODE_DPLL_ABE, 0, 3, PLL_MN_POWER_BYPASS);
	wait_on_value(BIT0, 0, CM_IDLEST_DPLL_ABE, LDELAY);

	/* Disable autoidle */
	sr32(CM_AUTOIDLE_DPLL_ABE, 0, 3, 0x0);

	sr32(CM_CLKSEL_DPLL_ABE, 8, 11, dpll_param_p->m);
	sr32(CM_CLKSEL_DPLL_ABE, 0, 6, dpll_param_p->n);

	/* Force DPLL CLKOUTHIF to stay enabled */
	sr32(CM_DIV_M2_DPLL_ABE, 0, 32, 0x500);
	sr32(CM_DIV_M2_DPLL_ABE, 0, 5, dpll_param_p->m2);
	sr32(CM_DIV_M2_DPLL_ABE, 8, 1, 0x1);
	/* Force DPLL CLKOUTHIF to stay enabled */
	sr32(CM_DIV_M3_DPLL_ABE, 0, 32, 0x100);
	sr32(CM_DIV_M3_DPLL_ABE, 0, 5, dpll_param_p->m3);
	sr32(CM_DIV_M3_DPLL_ABE, 8, 1, 0x1);

	/* Lock the abe dpll */
	sr32(CM_CLKMODE_DPLL_ABE, 0, 3, PLL_LOCK);
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_ABE, LDELAY);
}

static void configure_usb_dpll(dpll_param *dpll_param_p)
{
	/* Select the 60Mhz clock 480/8 = 60*/
	sr32(CM_CLKSEL_USB_60MHz, 0, 32, 0x1);

	/* Unlock the USB dpll */
	sr32(CM_CLKMODE_DPLL_USB, 0, 3, PLL_MN_POWER_BYPASS);
	wait_on_value(BIT0, 0, CM_IDLEST_DPLL_USB, LDELAY);

	/* Disable autoidle */
	sr32(CM_AUTOIDLE_DPLL_USB, 0, 3, 0x0);

	sr32(CM_CLKSEL_DPLL_USB, 8, 11, dpll_param_p->m);
	sr32(CM_CLKSEL_DPLL_USB, 0, 6, dpll_param_p->n);

	/* Force DPLL CLKOUT to stay active */
	sr32(CM_DIV_M2_DPLL_USB, 0, 32, 0x100);
	sr32(CM_DIV_M2_DPLL_USB, 0, 5, dpll_param_p->m2);
	sr32(CM_DIV_M2_DPLL_USB, 8, 1, 0x1);
	sr32(CM_CLKDCOLDO_DPLL_USB, 8, 1, 0x1);

	/* Lock the usb dpll */
	sr32(CM_CLKMODE_DPLL_USB, 0, 3, PLL_LOCK);
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_USB, LDELAY);

	/* force enable the CLKDCOLDO clock */
	sr32(CM_CLKDCOLDO_DPLL_USB, 0, 32, 0x100);
}

void configure_core_dpll_no_lock(void)
{
	dpll_param *dpll_param_p = &core_dpll_param_ddr400mhz;

	/* Get the sysclk speed from cm_sys_clksel
	 * Set it to 38.4 MHz, in case ROM code is bypassed
	 */
	writel(0x7,CM_SYS_CLKSEL);

	/* CORE_CLK=CORE_X2_CLK/2, L3_CLK=CORE_CLK/2, L4_CLK=L3_CLK/2 */
	sr32(CM_CLKSEL_CORE, 0, 32, 0x110);

	/* Unlock the CORE dpll */
	sr32(CM_CLKMODE_DPLL_CORE, 0, 3, PLL_MN_POWER_BYPASS);
	wait_on_value(BIT0, 0, CM_IDLEST_DPLL_CORE, LDELAY);

	/* Disable autoidle */
	sr32(CM_AUTOIDLE_DPLL_CORE, 0, 3, 0x0);

	sr32(CM_CLKSEL_DPLL_CORE, 8, 11, dpll_param_p->m);
	sr32(CM_CLKSEL_DPLL_CORE, 0, 6, dpll_param_p->n);
	sr32(CM_DIV_M2_DPLL_CORE, 0, 5, dpll_param_p->m2);
	sr32(CM_DIV_M3_DPLL_CORE, 0, 5, dpll_param_p->m3);
	sr32(CM_DIV_M4_DPLL_CORE, 0, 5, dpll_param_p->m4);
	sr32(CM_DIV_M5_DPLL_CORE, 0, 5, dpll_param_p->m5);
	sr32(CM_DIV_M6_DPLL_CORE, 0, 5, dpll_param_p->m6);
	sr32(CM_DIV_M7_DPLL_CORE, 0, 5, dpll_param_p->m7);
}

void lock_core_dpll(void)
{
	/* Lock the core dpll */
	sr32(CM_CLKMODE_DPLL_CORE, 0, 3, PLL_LOCK);
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_CORE, LDELAY);
}

void lock_core_dpll_shadow(void)
{
	dpll_param *dpll_param_p = &core_dpll_param_ddr400mhz;
	u32 temp;
	temp = readl(CM_MEMIF_CLKSTCTRL);
	temp &= (~3);
	temp |= 2;
	writel(temp, CM_MEMIF_CLKSTCTRL);

	while(readl(CM_MEMIF_EMIF_1_CLKCTRL) & 0x30000)
		;

	while(readl(CM_MEMIF_EMIF_2_CLKCTRL) & 0x30000)
		;

	/* Lock the core dpll using freq update method */
	/*(CM_CLKMODE_DPLL_CORE) */
	writel(0x0A, 0x4A004120);

	/* CM_SHADOW_FREQ_CONFIG1: DLL_OVERRIDE = 1(hack), DLL_RESET = 1,
	 * DPLL_CORE_M2_DIV =1, DPLL_CORE_DPLL_EN = 0x7, FREQ_UPDATE = 1
	 */
	writel(0x70D | (dpll_param_p->m2 << 11), 0x4A004260);

	/* Wait for Freq_Update to get cleared: CM_SHADOW_FREQ_CONFIG1 */
	while((readl(0x4A004260) & 0x1) == 0x1)
		;

	/* Wait for DPLL to Lock : CM_IDLEST_DPLL_CORE */
	wait_on_value(BIT0, 1, CM_IDLEST_DPLL_CORE, LDELAY);
	//lock_core_dpll();

	while(readl(CM_MEMIF_EMIF_1_CLKCTRL) & 0x30000)
		;

	while(readl(CM_MEMIF_EMIF_2_CLKCTRL) & 0x30000)
		;

	writel(temp|3, CM_MEMIF_CLKSTCTRL);
}

static void enable_all_clocks(void)
{
	/* L4PER clocks */
	sr32(CM_L4PER_CLKSTCTRL, 0, 32, 0x2);
	sr32(CM_L4PER_DMTIMER10_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_DMTIMER10_CLKCTRL, LDELAY);
	sr32(CM_L4PER_DMTIMER11_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_DMTIMER11_CLKCTRL, LDELAY);
	sr32(CM_L4PER_DMTIMER2_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_DMTIMER2_CLKCTRL, LDELAY);
	sr32(CM_L4PER_DMTIMER3_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_DMTIMER3_CLKCTRL, LDELAY);
	sr32(CM_L4PER_DMTIMER4_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_DMTIMER4_CLKCTRL, LDELAY);
	sr32(CM_L4PER_DMTIMER9_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_DMTIMER9_CLKCTRL, LDELAY);

	/* GPIO clocks */
	sr32(CM_L4PER_GPIO2_CLKCTRL, 0 ,32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_GPIO2_CLKCTRL, LDELAY);
	sr32(CM_L4PER_GPIO3_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_GPIO3_CLKCTRL, LDELAY);
	sr32(CM_L4PER_GPIO4_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_GPIO4_CLKCTRL, LDELAY);
	sr32(CM_L4PER_GPIO4_CLKCTRL, 8, 1, 0x1);
	sr32(CM_L4PER_GPIO5_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_GPIO5_CLKCTRL, LDELAY);
	sr32(CM_L4PER_GPIO6_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_GPIO6_CLKCTRL, LDELAY);

	sr32(CM_L4PER_HDQ1W_CLKCTRL, 0, 32, 0x2);

	/* I2C clocks */
	sr32(CM_L4PER_I2C1_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_I2C1_CLKCTRL, LDELAY);
	sr32(CM_L4PER_I2C2_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_I2C2_CLKCTRL, LDELAY);
	sr32(CM_L4PER_I2C3_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_I2C3_CLKCTRL, LDELAY);
	sr32(CM_L4PER_I2C4_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_I2C4_CLKCTRL, LDELAY);

	sr32(CM_L4PER_MCBSP4_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_MCBSP4_CLKCTRL, LDELAY);

	/* MCSPI clocks */
	sr32(CM_L4PER_MCSPI1_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_MCSPI1_CLKCTRL, LDELAY);
	sr32(CM_L4PER_MCSPI2_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_MCSPI2_CLKCTRL, LDELAY);
	sr32(CM_L4PER_MCSPI3_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_MCSPI3_CLKCTRL, LDELAY);
	sr32(CM_L4PER_MCSPI4_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_MCSPI4_CLKCTRL, LDELAY);

	/* MMC clocks */
	sr32(CM_L3INIT_HSMMC1_CLKCTRL, 0, 2, 0x2);
	sr32(CM_L3INIT_HSMMC1_CLKCTRL, 24, 1, 0x1);
	//wait_on_value(BIT18|BIT17|BIT16, 0, CM_L3INIT_HSMMC1_CLKCTRL, LDELAY);
	sr32(CM_L3INIT_HSMMC2_CLKCTRL, 0, 2, 0x2);
	sr32(CM_L3INIT_HSMMC2_CLKCTRL, 24, 1, 0x1);
	//wait_on_value(BIT18|BIT17|BIT16, 0, CM_L3INIT_HSMMC2_CLKCTRL, LDELAY);
	sr32(CM_L4PER_MMCSD3_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT18|BIT17|BIT16, 0, CM_L4PER_MMCSD3_CLKCTRL, LDELAY);
	sr32(CM_L4PER_MMCSD4_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT18|BIT17|BIT16, 0, CM_L4PER_MMCSD4_CLKCTRL, LDELAY);
	sr32(CM_L4PER_MMCSD5_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_MMCSD5_CLKCTRL, LDELAY);

	/* UART clocks */
	sr32(CM_L4PER_UART1_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_UART1_CLKCTRL, LDELAY);
	sr32(CM_L4PER_UART2_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_UART2_CLKCTRL, LDELAY);
	sr32(CM_L4PER_UART3_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_UART3_CLKCTRL, LDELAY);
	sr32(CM_L4PER_UART4_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_L4PER_UART4_CLKCTRL, LDELAY);

	/* WKUP clocks */
	sr32(CM_WKUP_GPIO1_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_WKUP_GPIO1_CLKCTRL, LDELAY);
	sr32(CM_WKUP_TIMER1_CLKCTRL, 0, 32, 0x01000002);
	wait_on_value(BIT17|BIT16, 0, CM_WKUP_TIMER1_CLKCTRL, LDELAY);

	sr32(CM_WKUP_KEYBOARD_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_WKUP_KEYBOARD_CLKCTRL, LDELAY);

	sr32(CM_SDMA_CLKSTCTRL, 0, 32, 0x0);
	sr32(CM_MEMIF_CLKSTCTRL, 0, 32, 0x3);
	sr32(CM_MEMIF_EMIF_1_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_MEMIF_EMIF_1_CLKCTRL, LDELAY);
	sr32(CM_MEMIF_EMIF_2_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_MEMIF_EMIF_2_CLKCTRL, LDELAY);
	sr32(CM_D2D_CLKSTCTRL, 0, 32, 0x3);
	sr32(CM_L3_2_GPMC_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L3_2_GPMC_CLKCTRL, LDELAY);
	sr32(CM_L3INSTR_L3_3_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L3INSTR_L3_3_CLKCTRL, LDELAY);
	sr32(CM_L3INSTR_L3_INSTR_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L3INSTR_L3_INSTR_CLKCTRL, LDELAY);
	sr32(CM_L3INSTR_OCP_WP1_CLKCTRL, 0, 32, 0x1);
	wait_on_value(BIT17|BIT16, 0, CM_L3INSTR_OCP_WP1_CLKCTRL, LDELAY);

	/* WDT clocks */
	sr32(CM_WKUP_WDT2_CLKCTRL, 0, 32, 0x2);
	wait_on_value(BIT17|BIT16, 0, CM_WKUP_WDT2_CLKCTRL, LDELAY);

	/* Select DPLL PER CLOCK as source for SGX FCLK */
	sr32(CM_SGX_SGX_CLKCTRL, 24, 1, 0x1);

	/* Enable clocks for USB fast boot to work */
	sr32(CM_L3INIT_USBPHY_CLKCTRL, 0, 32, 0x301);
	sr32(CM_L3INIT_HSUSBOTG_CLKCTRL, 0, 32, 0x1);

	return;
}

/* must be called from sram or flash */
void prcm_init(void)
{
	u32 clk_index;
	/* Get the sysclk speed from cm_sys_clksel
	 * Set the CM_SYS_CLKSEL in case ROM code has not set
	 */
	writel(0x7,CM_SYS_CLKSEL);
	clk_index = readl(CM_SYS_CLKSEL);
	if (!clk_index)
		return;

	/* Configure all DPLL's at 100% OPP */
	configure_mpu_dpll(&mpu_dpll_param);
	configure_iva_dpll(&iva_dpll_param);
	configure_per_dpll(&per_dpll_param);
	configure_abe_dpll(&abe_dpll_param);
	configure_usb_dpll(&usb_dpll_param);

	enable_all_clocks();
}

