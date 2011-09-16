#ifndef DEFS_H
#define DEFS_H

#define MAX_PIDS        64
#define TOTAL_PIDS      3000

enum top {
    TOP_Open   = 0,
    TOP_Create = 1,
    TOP_Unlink = 2,
    TOP_Read   = 3,
    TOP_Write  = 4,
    TOP_Close  = 5,
    TOP_Exit   = 6,
    TOP_End    = 7
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
