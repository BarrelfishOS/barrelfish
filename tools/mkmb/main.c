#include <sys/types.h>
#include <sys/stat.h>

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "build_multiboot.h"
#include "config.h"

void usage(char *name) {
    fprintf(stderr, "usage: %s <config> <fdt blob> <fs root>"
                    " <load address> <output image>\n", name);
    exit(EXIT_FAILURE);
}

void fail(char *name) {
    perror(name);
    exit(EXIT_FAILURE);
}

void elf_fail(char *name) {
    fprintf(stderr, "%s: %s\n", name, elf_errmsg(elf_errno()));
    exit(EXIT_FAILURE);
}

/* Read the complete contents of a file. */
void *
load_file(const char *path, size_t *length) {
    FILE *file= fopen(path, "r");
    if(!file) fail("fopen");

    struct stat stat;
    if(fstat(fileno(file), &stat) < 0) fail("stat");

    char *buf= malloc(stat.st_size);
    if(!buf) fail("malloc");

    if(fread(buf, 1, stat.st_size, file) != stat.st_size) fail("fread");

    if(fclose(file) != 0) fail("fclose");

    *length= stat.st_size;
    return buf;
}

/* Load the ELF image for a component, and fill the relevant fields in the
 * configuration struct. */
int
load_component(char *basepath, size_t bplen, struct component_config *comp,
               char *buf) {
    if(bplen + comp->path_len >= PATH_MAX) {
        errno= ENAMETOOLONG;
        return -1;
    }

    /* Append the component path to the FS base path, and null terminate. */
    memcpy(basepath + bplen, buf + comp->path_start, comp->path_len);
    basepath[bplen + comp->path_len]= '\0';

    /* Canonicalise the path. */
    char path[PATH_MAX];
    fprintf(stderr, "%s\n", basepath);
    if(!realpath(basepath, path)) fail("relpath");

    /* Load the ELF */
    printf("Loading component %s\n", path);
    comp->image= load_file(path, &comp->image_size);

    return 0;
}

int
main(int argc, char *argv[]) {
    if(argc != 6) usage(argv[0]);

    const char *config_path= argv[1],
               *fdt_path=    argv[2],
               *base_path=   argv[3],
               *out_path=    argv[5];

    errno= 0;
    uint64_t allocbase= strtoull(argv[4], NULL, 0);
    if(errno) fail("strtoull");

    /* Load the configuration. */
    size_t config_size;
    char *config_raw= (char *)load_file(config_path, &config_size);

    /* Parse the configuration. */
    struct config *config= parse_config(config_raw, config_size);
    if(!config) exit(EXIT_FAILURE);

    /* Construct the working buffer for paths. */
    char basepath_buf[PATH_MAX];
    size_t basepath_len;
    strncpy(basepath_buf, base_path, PATH_MAX);
    basepath_len= strlen(base_path);

    /* Load the kernel ELF. */
    assert(config->kernel);
    if(load_component(basepath_buf, basepath_len,
                      config->kernel, config_raw) != 0) {
        fail("load_component");
    }

    /* Load all modules. */
    for(struct component_config *comp= config->first_module;
                                 comp != config->last_module;
                                 comp= comp->next) {
        if(load_component(basepath_buf, basepath_len, comp, config_raw) != 0)
            fail("load_component");
    }

    /* Load the FDT blob. */
    size_t fdt_size;
    void *fdt_blob= load_file(fdt_path, &fdt_size);

    /* Allocate the kernel load address. */
    config->kernel->image_address= allocbase;
    allocbase+= config->kernel->image_size;

#if 0
    /* Open the output file for writing. */
    FILE *outfile= fopen(out_path, "w");
    if(!outfile) fail("fopen");

    fclose(outfile);
#endif

    return EXIT_SUCCESS;
}
