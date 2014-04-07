#ifndef KERNEL_MEMSET_H
#define KERNEL_MEMSET_H

void *memset_pages(void *s, int c, size_t n);

void memset_128b_sse2_(void *s, int c, size_t n);
void memset_128b_(void *s, int c, size_t n);

#endif // KERNEL_MEMSET_H
