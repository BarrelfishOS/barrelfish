/*
 * Copyright (C) 2010 The Android Open Source Project
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
#include <aboot/io.h>
#include <omap4/mux.h>
#include <omap4/hw.h>
#include <omap4/omap4_rom.h>
#include "protocol.h"

#define WITH_MEMORY_TEST	0
#define WITH_FLASH_BOOT		0
#define WITH_SIGNATURE_CHECK	0

#define min(a,b) ( ((a) < (b)) ? (a) : (b) )

#if WITH_MEMORY_TEST
void memtest(void *x, unsigned count) {
	unsigned *w = x;
	unsigned n;
	count /= 4;

	printf("memtest write - %d\n",count);
	for (n = 0; n < count; n++) {
		unsigned chk = 0xa5a5a5a5 ^ n;
		w[n] = chk;
	}
	printf("memtest read\n");
	for (n = 0; n < count; n++) {
		unsigned chk = 0xa5a5a5a5 ^ n;
		if (w[n] != chk) {
			printf("ERROR @ %x (%x != %x)\n", 
				(unsigned) (w+n), w[n], chk);
			return;
		}
	}
	printf("OK!\n");
}
#endif

struct usb usb;

unsigned cfg_machine_type = 2791;

#if 0
static void dump(unsigned char *address, unsigned len, char *text)
{
	unsigned i, j, l;

	printf("\n\n%s\n", text);

	l = len % 16;
	if (l)
		l = len + 16 - l;
	else
		l = len;

	for (i = 0; i < l; i += 16) {

		printf("%08X: ", address + i);

		for (j = i; j < i + 16; j ++ ) {
			if (j < l)
				printf("%02X ", address[j]);
			else
				printf("   ");
			if (j == i + 8)
				printf ("-");
		}

		printf(" | ");

		for (j = i; j < i + 16; j ++ ) {
			if (j < l)
				printf("%c", address[j] >= 32 ? address[j] : '.');
			else
				printf(" ");
			if (j == i + 8)
				printf ("-");
		}

		printf("\n");
	}
}
#endif

#if WITH_SIGNATURE_CHECK
unsigned call_trusted(unsigned appid, unsigned procid, unsigned flag, void *args);

int verify(void *data, unsigned len, void *signature, unsigned rights) {
	struct {
		unsigned count;
		void *data;
		unsigned len;
		void *signature;
		unsigned rights;
	} args;
	args.count = 4;
	args.data = data;
	args.len = len;
	args.signature = signature;
	args.rights = rights;
	return call_trusted(12, 0, 0, &args);
}
#endif

#if WITH_FLASH_BOOT
int load_image(unsigned device, unsigned start, unsigned count, void *data)
{
	int (*rom_get_mem_driver)(struct mem_driver **io, u32 type);
	struct mem_driver *io = 0;
	struct mem_device local_md_device, *md = 0;
	struct read_desc rd;
	u16 options;
	u32 base;
	int z;

	if (get_omap_rev() >= OMAP_4460_ES1_DOT_0)
		base = PUBLIC_API_BASE_4460;
	else
		base = PUBLIC_API_BASE_4430;

	rom_get_mem_driver = API(base + PUBLIC_GET_DRIVER_MEM_OFFSET);
	z = rom_get_mem_driver(&io, device);
	if (z)
		return -1;

	md = &local_md_device;
	memset(md, 0, sizeof(struct mem_device));
	options = 0; // 1 = init phoenix pmic?
	md->initialized   = 0;
	md->device_type   = device;
	md->xip_device    = 0;
	md->search_size   = 0;
	md->base_address  = 0;
	md->hs_toc_mask   = 0;
	md->gp_toc_mask   = 0;
	md->boot_options  = &options;
	md->device_data   = (void*) 0x80000000;
	memset(md->device_data, 0, 2500);

	z = io->init(md);
	if (z)
		return -1;

	rd.sector_start = start;
	rd.sector_count = count;
	rd.destination = data;
	z = io->read(md, &rd);

	return 0;
}

int load_from_mmc(unsigned device, unsigned *len)
{
	load_image(device, 512, 512, (void*) CONFIG_ADDR_DOWNLOAD);
	*len = 256 * 1024;
	return 0;
}
#endif

int load_from_usb(unsigned *_len, unsigned *_addr)
{
	u32 len, addr, msg;

//	enable_irqs();

	if (usb_open(&usb)) {
		printf("failed to open usb\n");
		return -1;
	}

	msg = ABOOT_IS_READY;
	usb_write(&usb, &msg, sizeof(msg));

	for (;;) {
		len = addr = 0x0;

		printf("Ready\n");

		usb_read(&usb, &len, 4);
		if (len == ABOOT_NO_MORE_DATA) {
			printf("OK, no more data\n");
			break;
		}
		usb_read(&usb, &addr, 4);

		printf("Reading %d bytes to %08X\n", len, addr);

		*_addr = addr;
		*_len = len;

		for (;;) {
			if (usb_read(&usb, (void*)addr, min(len, CHUNK_SIZE))) {
				printf("usb_read failed\n");
				return -1;
			}
			if (len < CHUNK_SIZE)
				break;
			//printf(".");
			len -= CHUNK_SIZE;
			addr += CHUNK_SIZE;
		}
		printf("\n");
	}

	usb_close(&usb);
//	disable_irqs();
	return 0;
}

void aboot(unsigned *info)
{
#if WITH_FLASH_BOOT
	unsigned bootdevice;
#endif
    unsigned n, len, addr = CONFIG_ADDR_DOWNLOAD;

	board_mux_init();
	sdelay(100);

	scale_vcores();
	prcm_init();
	board_ddr_init();
	gpmc_init();
	board_late_init();

	serial_init();

	serial_puts("\n[ aboot second-stage loader ]\n\n");

	printf("MSV=%08x\n",*((unsigned*) 0x4A00213C));

#if WITH_MEMORY_TEST
	printf("Memory test, pass 1");
	memtest(0x82000000, 8*1024*1024);
	printf("Memory test, pass 2");
	memtest(0xA0208000, 8*1024*1024);
#endif

	printf("Memory test complete\n");
#if !WITH_FLASH_BOOT
	n = load_from_usb(&len, &addr);
#else
	if (info) {
		bootdevice = info[2] & 0xFF;
	} else {
		bootdevice = 0x45;
	}

	switch (bootdevice) {
	case 0x45: /* USB */
		serial_puts("boot device: USB\n\n");
		n = load_from_usb(&len);
		break;
	case 0x05:
	case 0x06:
		serial_puts("boot device: MMC\n\n");
		n = load_from_mmc(bootdevice, &len);
		break;
	default:
		serial_puts("boot device: unknown\n");
		for (;;) ;
	}
#endif
	printf("load complete\n");
	if (n) {
		serial_puts("*** IO ERROR ***\n");
	} else {
#if WITH_SIGNATURE_CHECK
		void *data = (void*) (addr);
		void *sign = (void*) (addr + len - 280);
		if ((len < 281) || (len > (32*1024*1024)))
			goto fail_verify;
		len -= 280;

		n = verify(data, len, sign, 2);
		if (n != 0) {
		fail_verify:
			serial_puts("*** SIGNATURE VERIFICATION FAILED ***\n");
			for (;;) ;
		}
#endif
		printf("starting!\n");
		boot_image(cfg_machine_type, addr, len);
		serial_puts("*** BOOT FAILED ***\n");
	}

	for (;;) ;
}

