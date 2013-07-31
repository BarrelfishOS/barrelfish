#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/types.h>
#include <barrelfish/cap_predicates.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>

bool debug_all_the_things = false;

#define DEBUG_ALL_THE_THINGS(...) \
    do { \
        if (debug_all_the_things) \
            printf(__VA_ARGS__); \
    } while(0)


#define RUNS 20
#define MIN_RANGES 30
#define MAX_RANGES 100
#define MAX_ADDR_BITS 20
#define MAX_ADDR (1<<MAX_ADDR_BITS)
#define QUERY_COUNT 20

struct node {
    genvaddr_t address;
    size_t size;
    int tiebreak;
};

static inline size_t randrange(size_t begin, size_t end)
{
    return begin + rand() / (RAND_MAX / (end - begin + 1) + 1);
}

static void
get_ranges(size_t count, uint8_t max_addr_bits, struct cte *out)
{
    size_t gencount = 0;
    size_t sizebits;
    size_t size;
    genvaddr_t max_addr = 1ULL<<max_addr_bits;
    while (gencount < count) {
        sizebits = randrange(1,max_addr_bits-2);
        size = 1ULL<<sizebits;
        genvaddr_t begin = randrange(max_addr/10, max_addr-max_addr/4);
        genvaddr_t end = begin + size;
        if (end > max_addr) {
            continue;
        }
        bool valid = true;
        for (int j = 0; j < gencount; j++) {
            genvaddr_t r_addr = get_address(&(out[j].cap));
            size_t r_size = get_size(&(out[j].cap));
            genvaddr_t r_end = r_addr + r_size;
            if (begin < r_addr && end > r_addr && end < r_end) {
                valid = false;
                break;
            }
            if (begin > r_addr && begin < r_end && end > r_end) {
                valid = false;
                break;
            }
        }
        if (valid) {
            memset(&out[gencount], 0, sizeof(struct cte));
            out[gencount].cap.type = ObjType_RAM;
            out[gencount].cap.rights = CAPRIGHTS_ALLRIGHTS;
            out[gencount].cap.u.ram = (struct RAM) { .base = begin, .bits = sizebits };
            gencount++;
        }
    }
}

static inline size_t min_count(size_t *counts, size_t countcount)
{
    size_t min = (size_t)-1;
    for (int i = 0; i < countcount; i++) {
        if (counts[i] < min)
            min = counts[i];
    }
    return min;
}

__attribute__((unused))
static void dump_ranges(struct cte *ranges, size_t count)
{
    for (int i = 0; i < count; i++) {
        printf("address = %"PRIxGENVADDR"\nsize=%d\n",
                ranges[i].cap.u.ram.base, ranges[i].cap.u.ram.bits);
    }
}

extern struct cte *mdb_root;
int main(int argc, char *argv[])
{
    int r = 0;
    for (int run = 0; run < RUNS; run++) {
        putchar('-'); fflush(stdout);
        size_t count = randrange(MIN_RANGES, MAX_RANGES);
        struct cte ranges[count];
        get_ranges(count, MAX_ADDR_BITS, ranges);
        //dump_ranges(ranges, count);
        set_init_mapping(ranges, count);
        r = mdb_check_invariants();
        assert(!r); // invariants should hold at this point
        for (int i = 0; i < QUERY_COUNT; i++) {
            // check what happens if we remove the root node
            remove_mapping(mdb_root);
            r = mdb_check_invariants();
            if(r) {
                printf("===================\nINVARIANT %d FAILED\n===================\n", r);
                mdb_dump_all_the_things();
            }
        }
        // empty tree
        memset(ranges, 0, count * sizeof(struct cte));
        mdb_root = NULL;
    }
    printf("Everything ok\n");
    return 0;
}
