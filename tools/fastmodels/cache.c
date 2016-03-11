#include <stdint.h>

/* A one-bit mask at bit n */
#define BIT(n) (1ULL << (n))

/* An n-bit mask, beginning at bit 0 */
#define MASK(n) (BIT(n) - 1)

/* An n-bit field selector, beginning at bit m */
#define FIELD(m,n,x) (((x) >> m) & MASK(n))

static inline uint64_t
read_clidr_el1(void) {
    uint64_t clidr_el1;
    __asm volatile("mrs %[clidr_el1], clidr_el1" :
                    [clidr_el1] "=r" (clidr_el1));
    return clidr_el1;
}

static inline uint64_t
read_ccsidr_el1(void) {
    uint64_t ccsidr_el1;
    __asm volatile("mrs %[ccsidr_el1], ccsidr_el1" :
                    [ccsidr_el1] "=r" (ccsidr_el1));
    return ccsidr_el1;
}

static inline void
write_csselr_el1(int level, int instruction) {
    /* Register format:
     * 31 4 | 3   1 | 0
     * RES0 | level | instruction
     */
    uint64_t x= (instruction & 0x1) | ((level & 0x7) << 1);
    __asm volatile("msr csselr_el1, %0" : : "r" (x));
}

#define CTYPE(n,x) FIELD(3 * (n-1), 3 * (n-1) + 2, x)

enum armv8_cache_type {
    ARMv8_CACHE_NONE    = 0,
    ARMv8_CACHE_IONLY   = 1,
    ARMv8_CACHE_DONLY   = 2,
    ARMv8_CACHE_ID      = 3,
    ARMv8_CACHE_UNIFIED = 4,
};

static inline int
clz(uint64_t x) {
    int r;
    __asm volatile("clz %[r], %[x]" : [r] "=r"(r) : [x] "r"(x));
    return r;
}

static inline int
log2i(uint64_t x) {
    return 64 - clz(x-1);
}

void
invalidate_caches(void) {
    uint64_t clidr= read_clidr_el1();
    int loc= FIELD(24, 26, clidr);

    /* Invalidate all instruction caches to point of unification. */
    __asm volatile("ic iallu");

    /* Invalidate all data caches up to the point of coherence. */
    for(int level= 1; level <= loc; level++) {
        int ctype= CTYPE(level, clidr);

        /* Only worry about levels with a data cache. */
        if(ctype == ARMv8_CACHE_DONLY ||
           ctype == ARMv8_CACHE_ID ||
           ctype == ARMv8_CACHE_UNIFIED) {
            /* Read the data cache size & associativity. */
            write_csselr_el1(level-1, 0);
            uint64_t ccsidr= read_ccsidr_el1();
            int sets=     FIELD(13, 15, ccsidr) + 1,
                assoc=    FIELD( 3, 10, ccsidr) + 1,
                linebits= FIELD( 0,  3, ccsidr) + 4;

            /* Calculate the field offsets for the invalidate operation. */
            int setbits=   log2i(sets),
                assocbits= log2i(assoc);

            for(int w= 0; w < assoc; w++) {
                for(int s= 0; s < sets; s++) {
                    uint64_t op=
                        ((w & MASK(assocbits)) << (32-assocbits)) |
                        ((s & MASK(setbits)) << linebits) |
                        ((level & MASK(3)) << 1);
                    __asm volatile("dc isw, %[op]" : : [op] "r" (op));
                }
            }
        }
    }
}
