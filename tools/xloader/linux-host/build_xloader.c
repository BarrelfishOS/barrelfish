#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>

#if 0
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/sysmacros.h>
#include <asm/boot.h>
#endif


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
     */
    buf[514] = 0x48;
    buf[515] = 0x64;
    buf[516] = 0x72;
    buf[517] = 0x53;

    /*
     * set the number of setup sectors in addition to the boot sector.
     * this is SETUP_SECTORS - 1
     */
    buf[0x1f1] = SETUP_SECTORS-1;

    /*
     * set the size of the bootloader
     */
    buf[0x1f4] = sys_size;
    buf[0x1f5] = sys_size >> 8;
    buf[0x1f6] = sys_size >> 16;
    buf[0x1f7] = sys_size >> 24;

    if (fwrite(buf, 1, HEADER_SIZE, stdout) != HEADER_SIZE) {
        printf("Writing setup failed");
        return -1;
    }

    /* Copy the kernel code */
    if (fwrite(kernel, 1, sb.st_size, stdout) != sb.st_size) {
        printf("Writing setup failed");
        return -1;
    }

    close(fd);

    return 0;
}
