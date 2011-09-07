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
    TOP_Exit,
    TOP_End
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

#ifdef __linux__
struct _replay_eventrec__struct {
    uint8_t op;
    uint32_t fnumsize;
    uint8_t fd;
    uint8_t mode;
    uint32_t fline;
    uint16_t pid;
};
typedef struct _replay_eventrec__struct replay_eventrec_t;
#endif

#endif
