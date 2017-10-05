#define _USE_XOPEN /* for strdup() */
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/cpu_arch.h> // for CURRENT_CPU_TYPE
#include <barrelfish/spawn_client.h>

#include <spawndomain/spawndomain.h>
#include <vfs/vfs.h>

#include "kaluga.h"
#include "debug.h"

#define MAX_DRIVER_MODULES 128

static struct module_info modules[MAX_DRIVER_MODULES];

inline bool is_auto_driver(struct module_info* mi) {
    return mi->argc > 1 && strcmp(mi->argv[1], "auto") == 0;
}

inline bool is_started(struct module_info* mi)
{
    return (mi->num_started);
}

inline bool can_start(struct module_info* mi)
{
    return (mi->allow_multi || (mi->num_started == 0));
}

void set_start_function(char* binary, module_start_fn start_fn)
{
    struct module_info* mi = find_module(binary);
    if (mi != NULL) {
        mi->start_function = start_fn;
    }
}

void set_started(struct module_info* mi)
{
    mi->num_started += 1;
    assert(mi->allow_multi || (mi->num_started == 1));
}

void set_multi_instance(struct module_info* mi, uint8_t is_multi)
{
    mi->allow_multi = (is_multi != 0);
}

void set_core_id_offset(struct module_info* mi, coreid_t offset)
{
    mi->coreoffset = offset;
}

struct capref *get_did_ptr(struct module_info *mi)
{
    return (mi->did + mi->num_started);
}

coreid_t get_core_id_offset(struct module_info* mi)
{
    return mi->coreoffset * mi->num_started;
}

struct module_info* find_module(char *binary)
{
    assert(binary != NULL);
    bool found = false;
    struct module_info* si = NULL;

    for (size_t i=0; i< MAX_DRIVER_MODULES; i++) {
        si = &modules[i];
        if (si->binary != NULL && strcmp(si->binary, binary) == 0) {
            found = true;
            break;
        }
    }

    return (found) ? si : NULL;
}

/**
 * @brief finds corectrl driver module for a given CPU type.
 *
 * @param cpu_type  the CPU type
 *
 * @return pointer to the corectrl module, or NULL if not existing
 */
struct module_info* find_corectrl_for_cpu_type(enum cpu_type cpu_type)
{
    switch(cpu_type) {
    case CPU_K1OM:
    case CPU_X86_64:
    case CPU_X86_32:
    case CPU_ARM7:
    case CPU_ARM8:
        return find_module("corectrl");
        break;
    default:
        return NULL;
        break;
    }
}

static void parse_module(char* line, struct module_info* si)
{
    si->complete_line = strdup(line);

    char* path_end = strchr(line, ' ');
    if (path_end == NULL) {
        path_end = line + strlen(line);
    }
    size_t path_size = path_end-line;
    si->path = malloc(path_size+1);
    strncpy(si->path, line, path_size);
    si->path[path_size] = '\0';

    char* binary_start = strrchr(si->path, '/');
    si->binary = strdup(binary_start+1); // exclude /
    memset(si->did, 0, sizeof(si->did));
    si->start_function = default_start_function;
    si->num_started = 0;
    char* cmdstart = line + path_size - strlen(si->binary);
    si->args = strdup(line + path_size);

    // cmdargs will be tagged with \0
    si->cmdargs = strdup(cmdstart);
    si->argc = spawn_tokenize_cmdargs(si->cmdargs, si->argv,
                                      ARRAY_LENGTH(si->argv));
}

/**
 * \brief Parses bootmodules and stores info in
 * boot_modules.
 */
static errval_t parse_modules(char* bootmodules)
{
    assert(bootmodules != NULL);

    size_t entry = 0;
    char* bm = strdup(bootmodules);

    static const char* delim = "\n";
    char* line = strtok(bm, delim);

    while (line != NULL && entry < MAX_DRIVER_MODULES) {
        struct module_info* si = &modules[entry++];
        parse_module(line, si);
        KALUGA_DEBUG("found boot module:\n%s\n%s\n%s (%d)\n", si->binary, si->path, si->cmdargs, si->argc);

        line = strtok(NULL, delim);
    }
    free(bm);

    if (line == NULL) {
        return SYS_ERR_OK;
    }
    else {
        return KALUGA_ERR_PARSE_MODULES;
    }
}

/**
 * \brief Open bootmodules file and read it in
 */
static char* get_bootmodules(void)
{
    errval_t err;

    // open bootmodules file and read it in
    vfs_handle_t vh;
    err = vfs_open("/bootmodules", &vh);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to open /bootmodules");
    }

    struct vfs_fileinfo info;
    err = vfs_stat(vh, &info);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to stat /bootmodules");
    }

    char *bootmodules = malloc(info.size + 1);
    if (bootmodules == NULL) {
        USER_PANIC_ERR(LIB_ERR_MALLOC_FAIL,
                       "failed to allocate memory for bootmodules");
    }
    size_t bootmodules_len;
    err = vfs_read(vh, bootmodules, info.size, &bootmodules_len);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to read /bootmodules");
    } else if (bootmodules_len == 0) {
        USER_PANIC_ERR(err, "/bootmodules is empty");
    } else if (bootmodules_len != info.size) {
        USER_PANIC_ERR(err, "unexpected short read of /bootmodules");
    }

    err = vfs_close(vh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not close bottmodules file");
    }

    // terminate as a string
    bootmodules[bootmodules_len] = '\0';
    return bootmodules;
}

/**
 * \brief Set an initial default environment for our boot-time children
 */
void init_environ(void)
{
    int r;

    /* PATH=/build-arch/sbin */
    char pathstr[64];
    snprintf(pathstr, sizeof(pathstr), "/" BF_BINARY_PREFIX "%s/sbin",
             cpu_type_to_archstr(CURRENT_CPU_TYPE));
    pathstr[sizeof(pathstr) - 1] = '\0';
    r = setenv("PATH", pathstr, 0);
    if (r != 0) {
        USER_PANIC("failed to set PATH");
    }

    /* HOME=/ */
    r = setenv("HOME", "/", 0);
    if (r != 0) {
        USER_PANIC("failed to set HOME");
    }
}

errval_t init_boot_modules(void)
{
    char* bootmodules = get_bootmodules();
    errval_t err = parse_modules(bootmodules);
    free(bootmodules);

    return err;
}
