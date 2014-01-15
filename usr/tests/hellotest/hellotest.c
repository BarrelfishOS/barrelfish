#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>

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
    }

  return EXIT_SUCCESS;
}
