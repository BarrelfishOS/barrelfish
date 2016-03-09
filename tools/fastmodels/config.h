#ifndef __CONFIG_H
#define __CONFIG_H

#include <multiboot2.h>
#include <stdint.h>

/* The default inital stack size for the CPU driver, if it's not specified in
 * the configuration file. */
#define DEFAULT_STACK_SIZE 16384

struct component_config {
    /* The offset and length of the image path, and argument strings for this
     * component. */
    size_t path_start, path_len;
    size_t args_start, args_len;

    /* The size and target address of the ELF image. */
    size_t image_size, alloc_size;
    uint64_t image_address;

    /* A pointer to the module tag in the multiboot info image. */
    struct multiboot_tag_module_64 *tag;

    /* A pointer to the loaded image. */
    void *image;

    struct component_config *next;
};

struct config {
    /* The raw configuration file. */
    char *buf;

    /* The multiboot information structure. */
    void *multiboot;
    size_t multiboot_size, multiboot_alloc;

    /* Pointers (within the multiboot structure), to the memory map that needs
     * to be filled in after all allocation is finished. */
    struct multiboot_tag_efi_mmap *mmap_tag;
    void *mmap_start;

    /* The CPU driver load information. */
    struct component_config *kernel;
    uint64_t kernel_entry;
    uint64_t kernel_stack;
    uint64_t stack_size;

    /* The additional modules. */
    struct component_config *first_module, *last_module;
};

struct config *parse_config(char *buf, size_t size);

#endif /* __CONFIG_H */
