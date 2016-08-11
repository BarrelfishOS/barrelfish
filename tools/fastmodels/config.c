/* Parse a Hagfish configuration file - which in turn is roughly a GRUB
 * menu.lst.  */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"

static inline int
iscomment(char c) {
    return c == '#';
}

static inline int
istoken(char c) {
    return !isspace(c) && !iscomment(c);
}

static size_t
skip_whitespace(const char *buf, size_t size, size_t start, int skip_newlines) {
    assert(start < size);
    size_t i;

    for(i= start;
        i < size && ((isspace(buf[i]) && buf[i] != '\n') ||
                     (skip_newlines && buf[i] == '\n'));
        i++);

    assert(start <= i);
    assert(i <= size);
    assert(i == size ||
           !isspace(buf[i]) ||
           (!skip_newlines && buf[i] == '\n'));
    return i;
}

static size_t
find_eol(const char *buf, size_t size, size_t start) {
    assert(start < size);
    size_t i;

    for(i= start; i < size && buf[i] != '\n'; i++);

    assert(start <= i);
    assert(i <= size);
    assert(i == size || buf[i] == '\n');
    return i;
}

static size_t
find_token(const char *buf, size_t size, size_t start, int skip_newlines) {
    assert(start < size);
    size_t i= start;

    while(i < size && !istoken(buf[i])) {
        if(isspace(buf[i])) {
            /* Skip whitespace. */
            i= skip_whitespace(buf, size, i, skip_newlines);
        }
        else {
            /* Find the newline. */
            i= find_eol(buf, size, i);
            /* Skip over it, if not at EOF. */
            if(i < size) i++;
        }
    }

    assert(start <= i);
    assert(i <= size);
    assert(i == size || istoken(buf[i]));
    return i;
}

static size_t
get_token(const char *buf, size_t size, size_t start) {
    assert(start < size);
    assert(istoken(buf[start]));
    size_t i;

    for(i= start; i < size && istoken(buf[i]); i++);

    assert(start < i);
    assert(i <= size);
    assert(istoken(buf[i-1]));
    return i;
}

static int
get_cmdline(const char *buf, size_t size, size_t *cursor,
            size_t *cstart, size_t *clen, size_t *astart, size_t *alen) {
    assert(*cursor < size);
    *cursor= find_token(buf, size, *cursor, 0);
    if(!istoken(buf[*cursor])) {
        fprintf(stderr, "Missing command line\n");
        return 0;
    }
    *astart= *cstart= *cursor; /* Path starts here. */
    *cursor= get_token(buf, size, *cursor);
    *clen= *cursor - *cstart; /* Path ends here. */
    assert(*clen <= size - *cstart);
    *cursor= find_eol(buf, size, *cursor);
    *alen= *cursor - *astart;
    assert(*alen <= size - *astart); /* Arguments end here. */

    return 1;
}

struct config *
parse_config(char *buf, size_t size) {
    size_t cursor= 0;
    struct config *cfg;

    cfg= calloc(1, sizeof(struct config));
    if(!cfg) {
        fprintf(stderr, "calloc: %s\n", strerror(errno));
        goto parse_fail;
    }
    cfg->buf= buf;
    cfg->stack_size= DEFAULT_STACK_SIZE;

    while(cursor < size) {
        cursor= find_token(buf, size, cursor, 1);
        if(cursor < size) {
            size_t tstart= cursor, tlen;

            assert(istoken(buf[cursor]));
            cursor= get_token(buf, size, cursor);
            tlen= cursor - tstart;
            assert(tlen <= size - cursor);

            if(!strncmp("title", buf+tstart, 5)) {
                /* Ignore the title. */
                assert(cursor < size);
                cursor= find_eol(buf, size, cursor);
            }
            else if(!strncmp("stack", buf+tstart, 5)) {
                char arg[10];
                size_t astart, alen;

                cursor= skip_whitespace(buf, size, cursor, 0);
                if(!istoken(buf[cursor])) {
                    fprintf(stderr, "Expected stack size\n");
                    goto parse_fail;
                }
                astart= cursor;

                cursor= get_token(buf, size, cursor);
                alen= cursor - astart;
                assert(alen <= size - cursor);

                if(alen > 9) {
                    fprintf(stderr, "Stack size field too long\n");
                    goto parse_fail;
                }

                memcpy(arg, buf+astart, alen);
                arg[alen]= '\0';
                cfg->stack_size= strtoul(arg, NULL, 10);
            }
            else if(!strncmp("kernel", buf+tstart, 6)) {
                if(cfg->kernel) {
                    fprintf(stderr, "Kernel defined twice\n");
                    goto parse_fail;
                }

                cfg->kernel= calloc(1, sizeof(struct component_config));
                if(!cfg->kernel) {
                    fprintf(stderr, "calloc: %s\n", strerror(errno));
                    goto parse_fail;
                }

                /* Grab the command line. */
                if(!get_cmdline(buf, size, &cursor,
                                &cfg->kernel->path_start,
                                &cfg->kernel->path_len,
                                &cfg->kernel->args_start,
                                &cfg->kernel->args_len))
                    goto parse_fail;
            }
            else if(!strncmp("module", buf+tstart, 6)) {
                struct component_config *module=
                    calloc(1, sizeof(struct component_config));
                if(!module) {
                    fprintf(stderr, "calloc, %s\n", strerror(errno));
                    goto parse_fail;
                }

                /* Grab the command line. */
                if(!get_cmdline(buf, size, &cursor,
                                &module->path_start,
                                &module->path_len,
                                &module->args_start,
                                &module->args_len))
                    goto parse_fail;

                if(cfg->first_module) {
                    assert(cfg->last_module);
                    cfg->last_module->next= module;
                    cfg->last_module= module;
                }
                else {
                    assert(!cfg->last_module);
                    cfg->first_module= module;
                    cfg->last_module= module;
                }
            }
            else {
                fprintf(stderr,
                           "Unrecognised entry \"%.*s\", skipping line.\n",
                           (int)tlen, buf + tstart);
                cursor= find_eol(buf, size, cursor);
            }
        }
    }

    if(!cfg->kernel) {
        fprintf(stderr, "No kernel image specified\n");
        goto parse_fail;
    }

    return cfg;

parse_fail:
    if(cfg) {
        if(cfg->kernel) free(cfg->kernel);

        struct component_config *cmp= cfg->first_module;
        while(cmp) {
            struct component_config *next= cmp->next;
            free(cmp);
            cmp= next;
        }

        free(cfg);
    }
    return NULL;
}
