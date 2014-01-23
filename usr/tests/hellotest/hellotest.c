#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#define ITERATIONS 1000000000

int main(int argc, char *argv[])
{
    bench_init();
    while(1) {
        uint64_t start = bench_tsc();
        for (volatile int i = 0; i < ITERATIONS; i++);
        uint64_t end = bench_tsc();
        debug_printf("Time for loop-iteration: %"PRIu64" ticks / %"PRIu64" ms\n",
               end-start, bench_tsc_to_ms(end-start));
        struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();

        uintptr_t id;
        errval_t err = mc->vtbl.get_arch_core_id(mc, &id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "get_core_id failed.");
        }
        else{
            printf("%s:%s:%d: core_id = %lu\n",
                   __FILE__, __FUNCTION__, __LINE__, id);
        }
    }

  return EXIT_SUCCESS;
}
