
CC=gcc
CFLAGS=-O2 -std=gnu99 -I. -I../../include/arch/x86_64/ -I../../include/arch/x86/
DEPS=bench.h

all: gups gups_lcg

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

gups: single_cpu.o core_single_cpu.o common.o bench_ctl.o
	$(CC) -o gups single_cpu.o core_single_cpu.o common.o bench_ctl.o $(CFLAGS)

gups_lcg: single_cpu_lcg.o core_single_cpu_lcg.o common.o bench_ctl.o
	$(CC) -o gups_lcg single_cpu_lcg.o core_single_cpu_lcg.o common.o bench_ctl.o $(CFLAGS)
