#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include "elf.h"

#define BASE_PAGE_SIZE 0x1000

#define MAX_MODULES 256

static char *kernel_symbol_prefix;
static char *kernel_cmd_line;
static char *module_symbol_prefix[MAX_MODULES];
static char *module_cmd_line[MAX_MODULES];

static void
die(const char * str,
    ...)
{
    va_list args;
    va_start(args, str);
    vfprintf(stderr, str, args);
    fputc('\n', stderr);
    exit(1);
}

static void
usage(void)
{
    die("Usage: build setup system [rootdev] [> image]");
}

/*
 * FORMAT:
 *
 * /k1om/sbin/binary offset length \n
 *
 *
 */
int
main(int argc,
     char ** argv)
{
    int got_kernel = 0;
    int n_modules = 0;
    int n_mmaps = 0;

    uint32_t offset = 0;

    if (argc < 3) {
        printf("Usage: %s <menu.lst> <output.dat>\n", argv[0]);
        return 0;
    }

    FILE *f = fopen(argv[1], "r");
    assert((f != NULL) && "Could not open input file");

    FILE *o = fopen(argv[2], "w");
    assert((o != NULL) && "Could not open output file");

    // Process menu.lst, generating definitions
    char cmd[1024], args[1024], image[1024];
    while (!feof(f)) {
        char line[1024];

        cmd[0] = args[0] = image[0] = line[0] = '\0';

        fgets(line, 1024, f);
        sscanf(line, "%s %s %[^\n]", cmd, image, args);

        printf("%cmd = %s, image = %s, %s args = %s\n", cmd, image, args)

        continue;
        if (!strcmp(cmd, "kernel")) {
            kernel_symbol_prefix = get_symbol_name_prefix(image);
            kernel_cmd_line = malloc(strlen(line) + 1);
            sprintf(kernel_cmd_line, "%s %s", image, args);
            fprintf(o, "extern char %s_start;\n", kernel_symbol_prefix);
            fprintf(o, "extern char %s_end;\n", kernel_symbol_prefix);
            got_kernel = 1;
        } else if (!strcmp(cmd, "module")) {
            assert(n_modules < MAX_MODULES);
            module_symbol_prefix[n_modules] = get_symbol_name_prefix(image);
            module_cmd_line[n_modules] = malloc(strlen(line) + 1);
            sprintf(module_cmd_line[n_modules], "%s %s", image, args);
            fprintf(o, "extern char %s_start;\n", module_symbol_prefix[n_modules]);
            fprintf(o, "extern char %s_end;\n", module_symbol_prefix[n_modules]);
            n_modules++;
        } else if (!strcmp(cmd, "mmap")) {
            uint64_t base, len;
            int type;
            sscanf(args, "%" SCNi64 " %" SCNi64 " %i",
                    &base, &len, &type);
            printf("Inserting MMAP %d: [0x%" PRIx64 ", 0x%" PRIx64 "], type %d\n",
                    n_mmaps, base, len, type);
            fprintf(o, "static uint64_t mbi_mmap%d[] = {0x%lx, 0x%lx, %d};\n",
                    n_mmaps, base, len, type);
            n_mmaps++;
        } else {
            bool iscmd = false;
            for (int i = 0; i < strlen(cmd); i++) {
                if (cmd[i] == '#') {
                    break;
                }
                if (!isspace(cmd[i])) {
                    iscmd = true;
                    break;
                }
            }
            if (iscmd) {
                printf("Ignoring command '%s'\n", cmd);
            }
        }
    }

#if 0
    /* Copy the setup code */
    file = fopen(argv[1], "r");
    if (!file)
        die("Unable to open `%s': %m", argv[1]);
    c = fread(buf, 1, sizeof(buf), file);
    if (ferror(file))
        die("read-error on `setup'");
    if (c < 1024)
        die("The setup must be at least 1024 bytes");
    if (buf[510] != 0x55 || buf[511] != 0xaa)
        die("Boot block hasn't got boot flag (0xAA55)");
    fclose(file);

    /* Pad unused space with zeros */
    setup_sectors = (c + 511) / 512;
    if (setup_sectors < SETUP_SECT_MIN)
        setup_sectors = SETUP_SECT_MIN;
    i = setup_sectors * 512;
    memset(buf + c, 0, i - c);

    /* Set the default root device */
    buf[508] = minor_root;
    buf[509] = major_root;

    fprintf(stderr, "Setup is %d bytes (padded to %d bytes).\n", c, i);
#endif
    /* Everything is OK */
    return 0;
}
