#ifndef COMMON_H
#define COMMON_H

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_allocator.h>

#define expect_success(e) expect_success_(e,__FILE__,__LINE__)

static inline void expect_success_(errval_t err, const char *file,
        int line)
{
    if (err_is_ok(err)) return;
    debug_printf("Operation on %s:%d failed unexpectedly\n", file, line);
    err_print_calltrace(err);
    abort();
}

void initialize_channel(const char *str, struct bulk_channel *channel,
        struct bulk_channel_callbacks *cb, struct waitset *ws,
        enum bulk_channel_direction dir,  size_t bufsz, size_t metasz,
        bool *done);

errval_t cb_bind_received(struct bulk_channel *channel);
extern bool is_no_copy;

#endif // ndef COMMON_H

