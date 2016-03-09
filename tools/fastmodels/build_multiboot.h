#ifndef __BUILD_MULTIBOOT_H
#define __BUILD_MULTIBOOT_H

#include <libelf.h>
#include "config.h"

void *create_multiboot2_info(struct config *cfg, Elf *elf, size_t mmap_size);

#endif /* __BUILD_MULTIBOOT_H */
