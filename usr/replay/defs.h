#ifndef DEFS_H
#define DEFS_H

#define MAX_PIDS        64

enum top {
    TOP_Open,
    TOP_Create,
    TOP_Unlink,
    TOP_Read,
    TOP_Write,
    TOP_Close,
    TOP_Exit
};

enum flags {
    FLAGS_RdOnly,
    FLAGS_WrOnly,
    FLAGS_RdWr,
};

struct trace_entry {
    enum top op;
    union {
        size_t fnum;
        size_t size;
    } u;
    int fd;
    enum flags mode;
    int pid;
    int fline;

    struct trace_entry *next;
};

#endif
