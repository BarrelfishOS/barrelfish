
#include <aboot/aboot.h>
#include <aboot/io.h>
#include <omap4/mux.h>
#include <omap4/hw.h>

/* syslib.c */

void sr32(u32 addr, u32 start_bit, u32 num_bits, u32 value)
{
	u32 tmp, msk = 0;
	msk = 1 << num_bits;
	--msk;
	tmp = readl(addr) & ~(msk << start_bit);
	tmp |=  value << start_bit;
	writel(tmp, addr);
}

u32 wait_on_value(u32 read_bit_mask, u32 match_value, u32 read_addr, u32 bound)
{
	u32 i = 0, val;
	do {
		++i;
		val = readl(read_addr) & read_bit_mask;
		if (val == match_value)
			return (1);
		if (i == bound)
			return (0);
	} while (1);
}

void sdelay(unsigned long loops)
{
	__asm__ volatile ("1:\n" "subs %0, %1, #1\n"
			  "bne 1b":"=r" (loops):"0"(loops));
}

void scale_vcores(void)
{
	/* For VC bypass only VCOREx_CGF_FORCE  is necessary and
	 * VCOREx_CFG_VOLTAGE  changes can be discarded
	 */
	/* PRM_VC_CFG_I2C_MODE */
	writel(0x0, 0x4A307BA8);
	/* PRM_VC_CFG_I2C_CLK */
	writel(0x6026, 0x4A307BAC);

	/* set VCORE1 force VSEL */
	/* PRM_VC_VAL_BYPASS) */
	writel(0x3A5512, 0x4A307BA0);

	writel(readl(0x4A307BA0) | 0x1000000, 0x4A307BA0);
	while(readl(0x4A307BA0) & 0x1000000)
		;

	/* PRM_IRQSTATUS_MPU */
	writel(readl(0x4A306010), 0x4A306010);


	/* FIXME: set VCORE2 force VSEL, Check the reset value */
	/* PRM_VC_VAL_BYPASS) */
	writel(0x295B12, 0x4A307BA0);
	writel(readl(0x4A307BA0) | 0x1000000, 0x4A307BA0);
	while(readl(0x4A307BA0) & 0x1000000)
		;

	/* PRM_IRQSTATUS_MPU */
	writel(readl(0x4A306010), 0x4A306010);

	/*set VCORE3 force VSEL */
	/* PRM_VC_VAL_BYPASS */
	writel(0x2A6112, 0x4A307BA0);

	writel(readl(0x4A307BA0) | 0x1000000, 0x4A307BA0);

	while(readl(0x4A307BA0) & 0x1000000)
		;

	/* PRM_IRQSTATUS_MPU */
	writel(readl(0x4A306010), 0x4A306010);
}
