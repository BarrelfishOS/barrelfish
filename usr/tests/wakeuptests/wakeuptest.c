#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <bench/bench.h>

// 5 seconds in us
#define DFLT_DELAY (60UL*1000*1000)

static cycles_t register_time;

static void handler(void *arg)
{
    cycles_t time_tsc = bench_tsc() - register_time;
    uint64_t time_ms = bench_tsc_to_ms(time_tsc);
    debug_printf("deferred event triggered: after %"PRIu64" ms\n", time_ms);
}

int main(void)
{
    errval_t err;
    struct deferred_event ev;
    deferred_event_init(&ev);
    bench_init();
    register_time = bench_tsc();
    err = deferred_event_register(&ev, get_default_waitset(), DFLT_DELAY,
            MKCLOSURE(handler, NULL));
    assert(err_is_ok(err));

    while (true) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            return 1;
        }
    }
    return 0;
}
