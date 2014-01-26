#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#define ITERATIONS 100000000

int main(int argc, char *argv[])
{
    bench_init();
    while(1) {

        uint64_t start = bench_tsc();
        for (volatile int i = 0; i < ITERATIONS; i++);
        uint64_t end = bench_tsc();

        printf("%lu %lu %lu\n", ITERATIONS/(end-start), end-start, bench_tsc_to_ms(end-start));
    }

  return EXIT_SUCCESS;
}
