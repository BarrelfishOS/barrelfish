CC=gcc
CFLAGS=-O2 -std=gnu99 -I. -I../../include/arch/x86_64/ -I../../include/arch/x86/ -L/mnt/scratch
DEPS=bench.h

ifeq ($(DUNE),1)
	DUNE=/mnt/scratch/skaestle/software/dune/libdune
	CFLAGS+=-I$(DUNE) -L$(DUNE) -ldune -DDUNE
endif

ALL= gups gups_lcg #gups_dune gups_lcg_dune
all: $(ALL)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

OBJS_COMMON=common.o bench_ctl.o
OBJS_BASE=single_cpu.o core_single_cpu.o
OBJS_LCG=single_cpu_lcg.o core_single_cpu_lcg.o

gups: $(OBJS_COMMON) $(OBJS_BASE)
	$(CC) -o $@ $^ $(CFLAGS)

gups_lcg: $(OBJS_COMMON) $(OBJS_LCG)
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -f $(ALL) *.o
