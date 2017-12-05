#include <sys/types.h> /* for off_t */
#include <sys/errno.h>
#include <sys/mman.h>
#include <barrelfish/systime.h>
#include <sys/time.h>

/* defined in barrelfish/debug.h */
void debug_printf(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Some targets provides their own versions of this functions.  Those
   targets should define REENTRANT_SYSCALLS_PROVIDED in TARGET_CFLAGS.  */

typedef int   fsopen_fn_t(const char *, int);
typedef int   fsread_fn_t(int, void *buf, size_t);
typedef int   fswrite_fn_t(int, const void *, size_t);
typedef int   fsclose_fn_t(int);
typedef off_t fslseek_fn_t(int, off_t, int);

#define FAIL_FN() \
do { \
    debug_printf("***** %s:%s() called. Something is probably wrong! Maybe " \
                 "you forgot to call vfs_init().\n", __FILE__,__FUNCTION__); \
    return -1; \
} while (0)

static int open_fail(const char *pathname, int flags)      { FAIL_FN(); }
static int read_fail(int fd, void *buf, size_t len)        { FAIL_FN(); }
static int write_fail(int fd, const void *buf, size_t len) { FAIL_FN(); }
static int close_fail(int fd)                              { FAIL_FN(); }
static off_t lseek_fail(int fd, off_t off, int whence)     { FAIL_FN(); }

static struct {
    fsopen_fn_t  *open;
    fsread_fn_t  *read;
    fswrite_fn_t *write;
    fsclose_fn_t *close;
    fslseek_fn_t *lseek;
} fs_ops = {
    .open = open_fail,
    .read = read_fail,
    .write = write_fail,
    .close = close_fail,
    .lseek = lseek_fail
};

/**
 * \brief register fs operation handlers
 *
 * The handlers are expected to follow POSIX conventions, i.e. they
 * should return -1 on error and setting errno according to the actual error.
 */
void newlib_register_fsops__(fsopen_fn_t *open_fn,
                        fsread_fn_t *read_fn,
                        fswrite_fn_t *write_fn,
                        fsclose_fn_t *close_fn,
                        fslseek_fn_t *lseek_fn)
{
    fs_ops.open  = open_fn  ? open_fn  : open_fail;
    fs_ops.read  = read_fn  ? read_fn  : read_fail;
    fs_ops.write = write_fn ? write_fn : write_fail;
    fs_ops.close = close_fn ? close_fn : close_fail;
    fs_ops.lseek = lseek_fn ? lseek_fn : lseek_fail;
}


ssize_t _write(int fd, const void *buf, size_t nbytes)
{
    return fs_ops.write(fd, buf, nbytes);
}

ssize_t _read(int fd, void *buf, size_t nbytes)
{
    return fs_ops.read(fd, buf, nbytes);
}

int _close(int fd)
{
    return fs_ops.close(fd);
}

int _open(const char *pathname, int flags, ...)
{
    return fs_ops.open(pathname, flags);
}

off_t lseek(int fd, off_t offset, int whence)
{
    return fs_ops.lseek(fd, offset, whence);
}

/* we don't provide an fstat function.  It's only use seems to be in:
 * stdio/makebuf.c:59, which we don't really care about. Programs can use libvfs
 * or libposixcompat for this functionality  */
off_t _fstat(int fd, void *stat)
{
    *__error() = EBADF;
    return -1;
}

/**
 * \brief Is current process tainted by uid or gid changes
 */
int issetugid(void)
{
    return 0;
}

void * mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    *__error() = EBADF;
    return MAP_FAILED;
}

int munmap(void *addr, size_t len)
{
    *__error() = EINVAL;
    return -1;
}

ssize_t _writev(int fd, const struct iovec *iov, int iovcnt)
{
    *__error() = EINVAL;
    return -1;
}

int access(const char *path, int amode) __attribute__ ((weak));
int access(const char *path, int amode)
{
    *__error() = EACCES;
    return -1;
}

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    if (tv) {
        uint64_t ns = systime_to_ns(systime_now());
        tv->tv_sec = ns / 1000000000L;
        tv->tv_usec = (ns / 1000L) % 1000000L;
        return 0;
    }
    *__error() = EINVAL;
    return -1;
}

int clock_gettime(clockid_t clock_id, struct timespec *tp)
{
    // For now, all clocks are the same
    if (tp) {
        uint64_t ns = systime_to_ns(systime_now());
        tp->tv_sec = ns / 1000000000L;
        tp->tv_nsec = ns % 1000000000L;
        return 0;
    }
    *__error() = EINVAL;
    return -1;
}
