/*
 * Copyright (C) 2011 The Android Open Source Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the 
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED 
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <aboot/aboot.h>
#include <aboot/bootimg.h>

static void boot_image_android(unsigned machtype, unsigned image, unsigned len)
{
	void (*entry)(unsigned, unsigned, unsigned);
	struct boot_img_hdr *hdr = (void*) image;
	unsigned psize, pmask, kactual;
	unsigned *tag = (void*) CONFIG_ADDR_ATAGS;

	if (len < sizeof(*hdr))
		return;

	psize = hdr->page_size;
	pmask = hdr->page_size - 1;

	if ((psize != 1024) && (psize != 2048) && (psize != 4096))
		return;

	if (len < psize)
		return;

	kactual = (hdr->kernel_size + pmask) & (~pmask);

	/* CORE */
	*tag++ = 2;
	*tag++ = 0x54410001;

	if (hdr->ramdisk_size) {
		*tag++ = 4;
		*tag++ = 0x54420005;
		*tag++ = image + psize + kactual;
		*tag++ = hdr->ramdisk_size;
	}

	if (hdr->cmdline && hdr->cmdline[0]) {
		/* include terminating 0 and word align */
		unsigned n = (strlen((void*) hdr->cmdline) + 4) & (~3);
		*tag++ = (n / 4) + 2;
		*tag++ = 0x54410009;
		memcpy(tag, hdr->cmdline, n);
		tag += (n / 4);
	}

	/* END */
	*tag++ = 0;
	*tag++ = 0;

	/* need to move the kernel away from the ramdisk-in-bootimg
	 * otherwise the ramdisk gets clobbered before it can be
	 * uncompressed.
	 */
	memcpy((void*) CONFIG_ADDR_KERNEL, (void *)image + psize, kactual);
	entry = (void*) CONFIG_ADDR_KERNEL;

	printf("kernel:   0x%x (%d bytes)\n",
	       CONFIG_ADDR_KERNEL, hdr->kernel_size);
	printf("ramdisk:  0x%x (%d bytes)\n",
	       image + psize + kactual, hdr->ramdisk_size);
	printf("atags:    0x%x\n", CONFIG_ADDR_ATAGS);
	printf("cmdline:  %s\n", hdr->cmdline);
	printf("machtype: %d\n", machtype);

	serial_puts("\nbooting...\n");
	entry(0, machtype, CONFIG_ADDR_ATAGS);

	serial_puts("\nreturned from kernel?\n");
	for (;;) ;
}

static void __attribute__((noreturn)) boot_image_binary(unsigned machtype, unsigned image, unsigned len)
{
	void (*entry)(unsigned, unsigned, unsigned);

	printf("jumping to 0x%x...\n", image);
	entry = (void*)image;
	entry(0, cfg_machine_type, CONFIG_ADDR_ATAGS);
	for (;;);
}

void boot_image(unsigned machtype, unsigned image, unsigned len)
{
	unsigned n;
	char *x = (void*) image;

	/* is it android image ? */
	for (n = 0; n < 8; n++)
		if (x[n] != "ANDROID!"[n]) 
			break;
	if (n == 8)
		return boot_image_android(machtype, image, len);

	/* may be, is it uImage ? */
	
	/* .... */

	/* no, plain binary */
	boot_image_binary(machtype, image, len);

	return /* :) */;
}
