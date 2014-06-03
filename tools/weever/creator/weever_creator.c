/**
 * \file
 * \brief Builder for the Xeon Phi Bootloader
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Description
 *
 * This program prepends a >1024 byte header to the Xeon Phi bootloader. The
 * header contains the necessary fields with the information filled in used by
 * the Xeon Phi bootstrap. The header contains the Linux bootinfo struct.
 */


#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>

/*
 * Size information for the header. The size of the header has to be at
 * least 1024 bytes!
 */
#define SETUP_SECTORS 2
#define SECTOR_SIZE 512
#define HEADER_SIZE (SETUP_SECTORS*SECTOR_SIZE)

unsigned char buf[HEADER_SIZE+1];

int main(int argc, char ** argv)
{
    if (argc != 2) {
        printf("Usage: build setup system > image");
        return -2;
    }

    struct stat sb;
    int fd = open(argv[1], O_RDONLY);

    if (fd < 0 || fstat(fd, &sb)) {
        printf("Error while opening file\n");
        return -1;
    }

    unsigned int sys_size = (sb.st_size + 15) / 16;

    void *kernel = mmap(NULL, sb.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if (kernel == MAP_FAILED){
        printf("Error while mapping system\n");
        return -1;
    }

    // clear the buffer
    memset(buf, 0, SETUP_SECTORS * SECTOR_SIZE);

    /*
     * the following assignments set special values in the bootinfo structure
     * of the header. The Xeon Phi bootloader reads this value to determine
     * the size and location of the executable.
     */

    /*
     * This is the signature. Without this the kernel does not boot.
     * Signature is reads "HdrS"
     */
    buf[514] = 0x48;
    buf[515] = 0x64;
    buf[516] = 0x72;
    buf[517] = 0x53;

    /*
     * set the number of setup sectors in addition to the boot sector.
     * this is SETUP_SECTORS - 1
     *
     * Note: Setup must be at least 1024 bytes long to have enough space
     *       for the boot info struct
     */
    buf[0x1f1] = SETUP_SECTORS-1;

    /*
     * set the size of the bootloader
     */
    buf[0x1f4] = sys_size;
    buf[0x1f5] = sys_size >> 8;
    buf[0x1f6] = sys_size >> 16;
    buf[0x1f7] = sys_size >> 24;

    /*
     * write the filled in header to the file
     */
    if (fwrite(buf, 1, HEADER_SIZE, stdout) != HEADER_SIZE) {
        printf("Writing setup failed");
        return -1;
    }

    /*
     * write the bootloader
     */
    if (fwrite(kernel, 1, sb.st_size, stdout) != sb.st_size) {
        printf("Writing setup failed");
        return -1;
    }

    close(fd);

    return 0;
}
