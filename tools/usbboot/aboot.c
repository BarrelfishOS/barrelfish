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

#define min(a,b) ( ((a) < (b)) ? (a) : (b) )

struct usb usb;

unsigned cfg_machine_type = 2791;

int load_from_usb(u32 *_len, u32 *_addr, u32 *_entry)
{
	u32 len, addr, entry, msg;

	if (usb_open(&usb)) {
		printf("failed to open usb\n");
		return -1;
	}

	msg = ABOOT_IS_READY;
	usb_write(&usb, &msg, sizeof(msg));

	for (;;) {
		len = addr = 0x0;

		usb_read(&usb, &len, 4);
		if (len == ABOOT_NO_MORE_DATA) {
			printf("OK, no more data\n");
			break;
		}
		usb_read(&usb, &addr, 4);
		usb_read(&usb, &entry, 4);

        printf("Entry point is %08X\n", entry);
		printf("Reading %d bytes to %08X\n", len, addr);

		*_addr = addr;
		*_len = len;
		*_entry = entry;

        u32 to_read=    len;
        void *read_ptr= (void *)addr;
        size_t chunk= 0;
        while(to_read > 0) {
            /* Read a chunk. */
            u32 this_chunk= min(to_read, CHUNK_SIZE);
			if(usb_read(&usb, read_ptr, this_chunk)) {
				printf("usb_read failed\n");
				return -1;
			}
			to_read  -= this_chunk;
			read_ptr += this_chunk;

            /* Acknowledge reception. */
            msg= chunk;
            usb_write(&usb, &msg, sizeof(msg));
            chunk++;
		}
	}

	usb_close(&usb);
	return 0;
}

static void __attribute__((noreturn))
boot_image(u32 entry_point) {
	void (*entry)(u32, u32, u32, u32)= (void *)entry_point;

	printf("jumping to 0x%08X...\n", entry_point);
	entry(0, 0, 0, 0);
	for (;;);
}

void __attribute__((noreturn))
aboot(unsigned *info) {
    unsigned n;
    u32 len, addr = CONFIG_ADDR_DOWNLOAD, entry;

	board_mux_init();
	sdelay(100);

	scale_vcores();
	prcm_init();
	board_ddr_init();
	gpmc_init();
	board_late_init();

	serial_init();

	serial_puts("\n[ aboot second-stage loader ]\n\n");

	n = load_from_usb(&len, &addr, &entry);
	printf("load complete\n");
	if (n) {
		serial_puts("*** IO ERROR ***\n");
	} else {
		printf("starting!\n");
		boot_image(entry);
	}

	for (;;) ;
}

