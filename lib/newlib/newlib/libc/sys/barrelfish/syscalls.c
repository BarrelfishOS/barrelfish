#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <errno.h>
#include <stdio.h>

size_t (*_libc_terminal_read_func)(char *, size_t);
size_t (*_libc_terminal_write_func)(const char *, size_t);

void (*_libc_exit_func)(int);
void (*_libc_assert_func)(const char *, const char *, const char *, int);

typedef void *(*morecore_alloc_func_t)(size_t bytes, size_t *retbytes);
morecore_alloc_func_t sys_morecore_alloc;

typedef void (*morecore_free_func_t)(void *base, size_t bytes);
morecore_free_func_t sys_morecore_free;

size_t terminal_write(const char *data, size_t length);
FILE *(*_libc_fopen_func)(const char *fname, const char *prot);

int execve(char *name, char **argv, char **env) {
  errno = ENOMEM;
  return(-1);
}

int fork(void) {
  return(-1);
}

clock_t times(struct tms *buf) {
  return(-1);
}

/* end of syscalls.c */
