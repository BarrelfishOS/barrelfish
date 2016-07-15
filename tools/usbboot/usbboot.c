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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdint.h>
#include <inttypes.h>
#include <fcntl.h>
#include <string.h>

// #include "usb.h"
#include <usb-linux.h>
#include <protocol.h>

#define min(a,b) (((a) < (b)) ? (a): (b))

static int print_error(int r)
{
	fprintf(stderr, "failed: %s\n", strerror(r));
	return r;
}

#define CHECK_ERROR(r) do { if (r) { return print_error((r)); } } while(0)

#define USBBOOT_MAX_CHUNKS 20

struct usb_load_chunk {
	uint32_t address;
	void *data;
	unsigned size;
};

int usb_boot(usb_handle usb, struct usb_load_chunk *chunks)
{
	uint32_t msg_boot = 0xF0030002;
	uint32_t msg_getid = 0xF0030003;
	uint32_t msg;
	uint8_t id[81];
	int i;
	int r;

#define OFF_CHIP	0x04
#define OFF_ID		0x0F
#define OFF_MPKH	0x26
	memset(id, 0xee, 81);
	fprintf(stderr,"reading ASIC ID\n");
	r = linux_usb_write(usb, &msg_getid, sizeof(msg_getid));
	CHECK_ERROR(r);

	r = linux_usb_read(usb, id, sizeof(id));
	CHECK_ERROR(r);

	fprintf(stderr,"CHIP: %02x%02x\n", id[OFF_CHIP+0], id[OFF_CHIP+1]);
	fprintf(stderr,"IDEN: ");
	for (i = 0; i < 20; i++)
		fprintf(stderr,"%02x", id[OFF_ID+i]);
	fprintf(stderr,"\nMPKH: ");
	for (i = 0; i < 32; i++)
		fprintf(stderr,"%02x", id[OFF_MPKH+i]);
	fprintf(stderr,"\nCRC0: %02x%02x%02x%02x\n",
		id[73], id[74], id[75], id[76]);
	fprintf(stderr,"CRC1: %02x%02x%02x%02x\n",
		id[77], id[78], id[79], id[80]);

	fprintf(stderr,"sending 2ndstage to target... %08x\n",msg_boot);
	r = linux_usb_write(usb, &msg_boot, sizeof(msg_boot));
	CHECK_ERROR(r);
        usleep(1);
	r = linux_usb_write(usb, &chunks[0].size, sizeof(chunks[0].size));
	CHECK_ERROR(r);
        usleep(1);
	r = linux_usb_write(usb, chunks[0].data, chunks[0].size);
	CHECK_ERROR(r);

        // sleep to make stuff work
        sleep(2);

	msg = 0;
	fprintf(stderr,"waiting for 2ndstage response...\n");
	r = linux_usb_read(usb, &msg, sizeof(msg));
	CHECK_ERROR(r);

	fprintf(stderr, "response is %x\n", msg);
	if (msg != ABOOT_IS_READY) {
		fprintf(stderr,"unexpected 2ndstage response\n");
		return -1;
	}

	sleep(1);

	for (i = 1; chunks[i].address; i ++) {

		if (i != 1)
			printf("\n");

		fprintf(stderr, "sending size = %d, ", chunks[i].size);
		r = linux_usb_write(usb, &chunks[i].size, sizeof(chunks[i].size));
		CHECK_ERROR(r);

		fprintf(stderr, "sending address = 0x%08X, ", chunks[i].address);
		r = linux_usb_write(usb, &chunks[i].address, sizeof(chunks[i].address));
		CHECK_ERROR(r);

		fprintf(stderr, "sending image ");
		for (;;) {
			r = linux_usb_write(usb, chunks[i].data, min(chunks[i].size, CHUNK_SIZE));
			CHECK_ERROR(r);
			if (chunks[i].size < CHUNK_SIZE)
				break;
			chunks[i].data += CHUNK_SIZE;
			chunks[i].size -= CHUNK_SIZE;
			usleep(1);
		}
		CHECK_ERROR(r);

		sleep(1);
	}

	fprintf(stderr, "\nstarting chunk at 0x%"PRIx32"\n", chunks[1].address);
	msg = ABOOT_NO_MORE_DATA;
	r = linux_usb_write(usb, &msg, sizeof(msg));
	CHECK_ERROR(r);
	
	return 0;
}


void *load_file(const char *file, unsigned *sz)
{
	void *data;
	struct stat s;
	int fd;
	
	fd = open(file, O_RDONLY);
	if (fd < 0)
		goto fail;

	if (fstat(fd, &s))
		goto fail;
	
	data = malloc(s.st_size);
	if (!data)
		goto fail;
	
	if (read(fd, data, s.st_size) != s.st_size) {
		free(data);
		goto fail;
	}
	
	close(fd);
	*sz = s.st_size;
	return data;
	
fail:
	fprintf(stderr, "Cannot read file '%s'\n", file);
	if (fd >= 0)
		close(fd);
	return 0;
}

extern unsigned char aboot_data[];
extern unsigned aboot_size;

int main(int argc, char **argv)
{
	usb_handle usb;
	int once = 1, i;
	int r;
	struct usb_load_chunk chunks[USBBOOT_MAX_CHUNKS], *current;
	char *p;
	char *aboot_cmdline = "--aboot=";
	int aboot_cmdline_sz = strlen(aboot_cmdline);

	memset(chunks, 0, sizeof(chunks));

	chunks[ 0 ].data = aboot_data;
	chunks[ 0 ].size = aboot_size;

	if (argc < 2) {
		fprintf(stderr,"usage: usbboot [ <2ndstage> ] <image>\n");
		return 0;
	}

	for (i = 1, current = chunks + 1;
	     i < argc && current - chunks < USBBOOT_MAX_CHUNKS;
	     i ++) {

		/* check if argument is 2nd stage bootloader name */
		if (strcmp(argv[i], "-a") == 0) {
			chunks[0].data = load_file(argv[i + 1], &chunks[0].size);
			i++;
			if (chunks[0].data == NULL)
				break;
			continue;
		}

		if (strncmp(argv[i], aboot_cmdline, aboot_cmdline_sz) == 0) {
			chunks[0].data = load_file(argv[i] + aboot_cmdline_sz, &chunks[0].size);
			if (chunks[0].data == NULL)
				break;
			continue;
		}
			
		p = strchr(argv[i], '=');
		if (p == NULL)
			p = strchr(argv[i], ':');
		if (p == NULL) {
			current->address = 0x82000000;
			fprintf(stderr, "Warning: using %x for '%s'\n", current->address, argv[i]);
			p = argv[i];
		}
		else {
			*p = '\0';
			current->address = strtoul(argv[i], NULL, 0);
			p++;	/* skip the ':' or '=' */
		}
		current->data = load_file(p, &current->size);
		if (current->data == NULL)
			break;
		current ++;
	}

	r = linux_usb_init();
	CHECK_ERROR(r);

	for (i = 0; i < USBBOOT_MAX_CHUNKS; i ++) {
		if (i && !chunks[i].address)
			break;
		printf("Chunk %d:\n", i);
		printf("\taddress = 0x%08X\n", chunks[i].address);
		printf("\tdata    = %p\n", chunks[i].data);
		printf("\tsize    = %d\n", chunks[i].size);
	}

	for (;;) {
		r = linux_usb_open(0x0451, 0xd010, &usb);
		if (r == 0 && usb) {
			r = usb_boot(usb, chunks);
			linux_usb_close(usb);
			break;
		}
		if (once) {
			once = 0;
			fprintf(stderr,"waiting for OMAP44xx device...\n");
		}
		usleep(250);
	}

	linux_usb_fini();

	return r;
}
