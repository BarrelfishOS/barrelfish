#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../include/grubmenu.h"

static int
skip_line(FILE *infile) {
    int c;

    do {
        c= fgetc(infile);
        if(c < 0) return c;
    } while(c != '\n');

    return 0;
}

static int
skip_whitespace(FILE *infile, int multiline) {
    int c;
    do {
        c = fgetc(infile);
        if(c < 0) return c;
        if(!multiline && c == '\n') break;
    } while(isspace(c));
    ungetc(c, infile);

    return 0;
}

static int
peek(FILE *f) {
    int c= fgetc(f);
    if(c >= 0) ungetc(c, f);
    return c;
}

#define MAXBUF 255

static int
read_token(FILE *infile, char *buf) {
    int c= peek(infile);
    if(c == EOF) return 0;
    if(c < 0) return c;

    while(isspace(c) || c == '#') {
        skip_whitespace(infile, 1);
        if(c == '#') skip_line(infile);
        c= peek(infile);
        if(c == EOF) return 0;
        if(c < 0) return c;
    }

    size_t i= 0;
    c= fgetc(infile);
    while(!isspace(c)) {
        assert(i < MAXBUF);
        buf[i]= c;
        i++;
        c= fgetc(infile);
        if(c == EOF) break;
        if(c < 0) return c;
    }
    if(c >= 0) ungetc(c, infile);

    buf[i]= '\0';
    return i + 1;
}

static int
read_line(FILE *infile, char *buf) {
    int c;

    int len= skip_whitespace(infile, 0);
    if(len < 0) return len;

    size_t i= 0;
    do {
        c= fgetc(infile);
        if(c < 0) return c;
        assert(i < MAXBUF);
        if(c != '\n') {
            buf[i]= c;
            i++;
        }
    } while(c != '\n');

    buf[i]= '\0';
    return i + 1;
}

typedef int (*reader_t)(FILE *, char *);

static int
read_string(FILE *f, reader_t reader, char **bufptr) {
    char buf[MAXBUF+1];

    size_t len= reader(f, buf);
    if(len < 0) return len;

    *bufptr= malloc(len);
    if(!*bufptr) {
        perror("malloc");
        return 0;
    }

    memcpy(*bufptr, buf, len);
    return len;
}

struct menu_lst *
read_menu_lst(const char *path) {
    int len;

    FILE *infile= fopen(path, "r");
    if(!infile) {
        perror("fopen");
        return NULL;
    }

    struct menu_lst *menu= calloc(sizeof(struct menu_lst), 1);
    if(!menu) {
        perror("malloc");
        return NULL;
    }

    char cmd[MAXBUF+1];
    while((len= read_token(infile, cmd)) > 0) {
        if(!strcmp(cmd, "title")) {
            len= read_string(infile, read_token, &menu->title);
            if(len <= 0) {
                fprintf(stderr, "Missing title value.\n");
                break;
            }
        }
        else if(!strcmp(cmd, "kernel")) {
            len= read_string(infile, read_token, &menu->kernel.path);
            if(len <= 0) {
                fprintf(stderr, "Missing kernel path.\n");
                break;
            }

            len= read_string(infile, read_line, &menu->kernel.args);
            if(len <= 0) {
                fprintf(stderr, "Missing kernel arguments.\n");
                break;
            }
        }
        else if(!strcmp(cmd, "image")) {
            len= read_string(infile, read_line, &menu->image);
            if(len <= 0) {
                fprintf(stderr, "Missing image specifier.\n");
                break;
            }
        }
        // handle "module" and "modulenounzip"
        else if(!strncmp(cmd, "module", 6)) {
            menu->nmodules++;

            menu->modules=
                realloc(menu->modules,
                        menu->nmodules * sizeof(struct menu_module));
            if(!menu->modules) {
                perror("realloc");
                return NULL;
            }

            struct menu_module *mod= &menu->modules[menu->nmodules - 1];

            len= read_string(infile, read_token, &mod->path);
            if(len <= 0) {
                fprintf(stderr, "Missing module path.\n");
                break;
            }

            len= read_string(infile, read_line, &mod->args);
            if(len <= 0) {
                fprintf(stderr, "Missing module arguments.\n");
                break;
            }
        }
        else if(!strcmp(cmd, "mmap")) {
            menu->mmap_len++;

            menu->mmap=
                realloc(menu->mmap,
                        menu->mmap_len * sizeof(struct menu_mmap_entry));
            if(!menu->mmap) {
                perror("realloc");
                return NULL;
            }

            struct menu_mmap_entry *entry= &menu->mmap[menu->mmap_len - 1];

            len= read_string(infile, read_token, &entry->name);
            if(len <= 0) {
                fprintf(stderr, "Missing MMAP entry name.\n");
                break;
            }

            char buf[MAXBUF+1];
            len= read_token(infile, buf);
            if(len <= 0) {
                fprintf(stderr, "Missing MMAP start address.\n");
                break;
            }
            errno= 0;
            entry->base= strtoull(buf, NULL, 0);
            if(errno) {
                fprintf(stderr, "Invalid MMAP start address.\n");
                break;
            }

            len= read_token(infile, buf);
            if(len <= 0) {
                fprintf(stderr, "Missing MMAP length.\n");
                break;
            }
            errno= 0;
            entry->length= strtoull(buf, NULL, 0);
            if(errno) {
                fprintf(stderr, "Invalid MMAP length.\n");
                break;
            }

            len= read_token(infile, buf);
            if(len <= 0) {
                fprintf(stderr, "Missing MMAP ID.\n");
                break;
            }
            errno= 0;
            entry->type= strtoul(buf, NULL, 0);
            if(errno) {
                fprintf(stderr, "Invalid MMAP ID.\n");
                break;
            }
        }
        else if(!strcmp(cmd, "timeout")) {
            char buf[MAXBUF+1];
            len= read_token(infile, buf);
            if(len <= 0) {
                fprintf(stderr, "Missing timeout value.\n");
                break;
            }
            errno= 0;
            menu->timeout= strtoul(buf, NULL, 0);
            if(errno) {
                fprintf(stderr, "Invalid timeout value.\n");
                break;
            }
        }
        else {
            fprintf(stderr, "Unknown command: %s\n", cmd);
        }
    }
    if(len < 0) {
        perror("read_token");
        return NULL;
    }

    if(fclose(infile)) {
        perror("fclose");
        return NULL;
    }

    return menu;
}
