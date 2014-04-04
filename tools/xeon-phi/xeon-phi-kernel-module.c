#include <linux/module.h>
#include <linux/version.h>
#include <linux/kernel.h>
#include <linux/types.h>
#include <linux/kdev_t.h>
#include <linux/fs.h>
#include <linux/device.h>
#include <linux/cdev.h>
#include <linux/uaccess.h>
#include <asm/io.h>

uint64_t mic_base_start = 0;
module_param(myint, uint64_t, 0);

uint8_t mic_id = 0;
module_param(myint, uint64_t, 0);

static uint8_t *mmiobase;

#define MEMBAR1_SIZE = (128 * 1024)

#define SBOX_OFFSET


static int32_t get_addresses()
{


}

int init_module(void) {
	printk(KERN_INFO "Xeon-Phi Module: Init\n");

	if ((mmiobase = ioremap(mic_base_start, MEMBAR1_SIZE)) == NULL) {
		printk(KERN_ERR "Mapping of MIC memory region failed\n");
		return -1;
	}

	/*
	 *       * A non 0 return means init_module failed; module can't be loaded.
	 *               */

	printk(KERN_INFO "Xeon-Phi Module: Done\n");
	return 0;
}

