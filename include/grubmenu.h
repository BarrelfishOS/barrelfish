#ifndef __GRUB_MENU_H
#define __GRUB_MENU_H

#include <stdint.h>

struct menu_module {
    char *path;
    char *args;
};

struct menu_mmap_entry {
    char *name;
    uint64_t base;
    uint64_t length;
    uint32_t type;
};

struct menu_lst {
    char *title;

    uint32_t timeout;

    struct menu_module kernel;

    uint32_t nmodules;
    struct menu_module *modules;

    uint32_t mmap_len;
    struct menu_mmap_entry *mmap;

    char *image;
};

struct menu_lst *read_menu_lst(const char *path);

#endif /* __GRUB_MENU_H */
