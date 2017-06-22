/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
/* No user fns here.  Pesch 15apr92. */

/* Barrelfish-specific version: copied from stdio/findfp.c.  We need to keep
 * this seperate so that we can fill appropriate (i.e., non vfs dependent)
 * functions for stdin/stdout/stderr */

//#include <_ansi.h>
//#include <reent.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
//#include <sys/lock.h>

#include "../../stdio/local.h" /* XXX: Ugly */

#ifdef _RENT_SMALL
#error "Not sure if _RENT_SMALL will work for barrelfish. Please check it"
#endif

#ifdef __LARGE64_FILES
#error "Not sure if _LARGE64_FILES will work for barrelfish. Please check it"
#endif


#ifdef _REENT_SMALL
const struct __sFILE_fake __sf_fake_stdin =
    {_NULL, 0, 0, 0, 0, {_NULL, 0}, 0, _NULL};
const struct __sFILE_fake __sf_fake_stdout =
    {_NULL, 0, 0, 0, 0, {_NULL, 0}, 0, _NULL};
const struct __sFILE_fake __sf_fake_stderr =
    {_NULL, 0, 0, 0, 0, {_NULL, 0}, 0, _NULL};
#endif

/* functions for stdin/stdout/stderr that do not depend on vfs
 * similar to: oldc/src/sys-barrelfish/sys_stdio.c */
size_t (*_libc_terminal_read_func)(char *, size_t);
size_t (*_libc_terminal_write_func)(const char *, size_t);

static _READ_WRITE_RETURN_TYPE
_DEFUN(term_read, (ptr, cookie, buf, n),
       struct _reent *ptr _AND
       void *cookie _AND
       char *buf _AND
       int n)
{
    if (_libc_terminal_read_func) {
        return _libc_terminal_read_func((char *)buf, n);
    } else {
        return -1;
    }
}

static _READ_WRITE_RETURN_TYPE
_DEFUN(read_fail, (ptr, cookie, buf, n),
       struct _reent *ptr _AND
       void *cookie _AND
       char *buf _AND
       int n)
{
    return -1;
}

static _READ_WRITE_RETURN_TYPE
_DEFUN(term_write, (ptr, cookie, buf, n),
       struct _reent *ptr _AND
       void *cookie _AND
       char const *buf _AND
       int n)
{
    if (_libc_terminal_write_func) {
        return _libc_terminal_write_func((char *)buf, n);
    } else {
        return -1;
    }
}

static _READ_WRITE_RETURN_TYPE
_DEFUN(write_fail, (ptr, cookie, buf, n),
       struct _reent *ptr _AND
       void *cookie _AND
       char const *buf _AND
       int n)
{
    return -1;
}

static _fpos_t
_DEFUN(seek_fail, (ptr, cookie, offset, whence),
       struct _reent *ptr _AND
       void *cookie _AND
       _fpos_t offset _AND
       int whence)
{
    return -1;
}


static int
_DEFUN(close_fail, (ptr, cookie),
       struct _reent *ptr _AND
       void *cookie)
{
    return  -1;
}

static _VOID
_DEFUN(std, (ptr, flags, file, data),
            FILE *ptr _AND
            int flags _AND
            int file  _AND
            struct _reent *data)
{
  ptr->_p = 0;
  ptr->_r = 0;
  ptr->_w = 0;
  ptr->_flags = flags;
  ptr->_flags2 = 0;
  ptr->_file = file;
  ptr->_bf._base = 0;
  ptr->_bf._size = 0;
  ptr->_lbfsize = 0;
  memset (&ptr->_mbstate, 0, sizeof (_mbstate_t));
  ptr->_cookie = ptr;
  ptr->_read = __sread;
#ifndef __LARGE64_FILES
  ptr->_write = __swrite;
#else /* __LARGE64_FILES */
  ptr->_write = __swrite64;
  ptr->_seek64 = __sseek64;
  ptr->_flags |= __SL64;
#endif /* __LARGE64_FILES */
  ptr->_seek = __sseek;
  ptr->_close = __sclose;
#if !defined(__SINGLE_THREAD__) && !defined(_REENT_SMALL)
  __lock_init_recursive (ptr->_lock);
  /*
   * #else
   * lock is already initialized in __sfp
   */
#endif

#ifdef __SCLE
  if (__stextmode (ptr->_file))
    ptr->_flags |= __SCLE;
#endif
}

struct _glue *
_DEFUN(__sfmoreglue, (d, n),
       struct _reent *d _AND
       register int n)
{
  struct _glue *g;
  FILE *p;

  g = (struct _glue *) _malloc_r (d, sizeof (*g) + n * sizeof (FILE));
  if (g == NULL)
    return NULL;
  p = (FILE *) (g + 1);
  g->_next = NULL;
  g->_niobs = n;
  g->_iobs = p;
  memset (p, 0, n * sizeof (FILE));
  return g;
}

/*
 * Find a free FILE for fopen et al.
 */

FILE *
_DEFUN(__sfp, (d),
       struct _reent *d)
{
  FILE *fp;
  int n;
  struct _glue *g;

  __sfp_lock_acquire ();

  if (!_GLOBAL_REENT->__sdidinit)
    __sinit (_GLOBAL_REENT);
  for (g = &_GLOBAL_REENT->__sglue;; g = g->_next)
    {
      for (fp = g->_iobs, n = g->_niobs; --n >= 0; fp++)
	if (fp->_flags == 0)
	  goto found;
      if (g->_next == NULL &&
	  (g->_next = __sfmoreglue (d, NDYNAMIC)) == NULL)
	break;
    }
  __sfp_lock_release ();
  d->_errno = ENOMEM;
  return NULL;

found:
  fp->_file = -1;		/* no file */
  fp->_flags = 1;		/* reserve this slot; caller sets real flags */
  fp->_flags2 = 0;
#ifndef __SINGLE_THREAD__
  __lock_init_recursive (fp->_lock);
#endif
  __sfp_lock_release ();

  fp->_p = NULL;		/* no current pointer */
  fp->_w = 0;			/* nothing to read or write */
  fp->_r = 0;
  fp->_bf._base = NULL;		/* no buffer */
  fp->_bf._size = 0;
  fp->_lbfsize = 0;		/* not line buffered */
  memset (&fp->_mbstate, 0, sizeof (_mbstate_t));
  /* fp->_cookie = <any>; */	/* caller sets cookie, _read/_write etc */
  fp->_ub._base = NULL;		/* no ungetc buffer */
  fp->_ub._size = 0;
  fp->_lb._base = NULL;		/* no line buffer */
  fp->_lb._size = 0;

  return fp;
}

/*
 * exit() calls _cleanup() through *__cleanup, set whenever we
 * open or buffer a file.  This chicanery is done so that programs
 * that do not use stdio need not link it all in.
 *
 * The name `_cleanup' is, alas, fairly well known outside stdio.
 */

_VOID
_DEFUN(_cleanup_r, (ptr),
       struct _reent *ptr)
{
  _CAST_VOID _fwalk(ptr, fclose);
  /* _CAST_VOID _fwalk (ptr, fflush); */	/* `cheating' */
}

#ifndef _REENT_ONLY
_VOID
_DEFUN_VOID(_cleanup)
{
  _cleanup_r (_GLOBAL_REENT);
}
#endif

/*
 * __sinit() is called whenever stdio's internal variables must be set up.
 */

_VOID
_DEFUN(__sinit, (s),
       struct _reent *s)
{
  __sinit_lock_acquire ();

  if (s->__sdidinit)
    {
      __sinit_lock_release ();
      return;
    }

  /* make sure we clean up on exit */
  s->__cleanup = _cleanup_r;	/* conservative */
  s->__sdidinit = 1;

  s->__sglue._next = NULL;
#ifndef _REENT_SMALL
  s->__sglue._niobs = 3;
  s->__sglue._iobs = &s->__sf[0];
#else
  s->__sglue._niobs = 0;
  s->__sglue._iobs = NULL;
  s->_stdin = __sfp(s);
  s->_stdout = __sfp(s);
  s->_stderr = __sfp(s);
#endif

  /*
   * Barrelfish doesn't support buffered stdin
   */
  std (s->_stdin,  __SRD | __SNBF, 0, s);

  /* On platforms that have true file system I/O, we can verify
     whether stdout is an interactive terminal or not, as part of
     __smakebuf on first use of the stream.  For all other platforms,
     we will default to line buffered mode here.  Technically, POSIX
     requires both stdin and stdout to be line-buffered, but tradition
     leaves stdin alone on systems without fcntl.  */
#ifdef HAVE_FCNTL
  std (s->_stdout, __SWR, 1, s);
#else
  std (s->_stdout, __SWR | __SLBF, 1, s);
#endif

  /* POSIX requires stderr to be opened for reading and writing, even
     when the underlying fd 2 is write-only.  */
  std (s->_stderr, __SRW | __SNBF, 2, s);

  /* Barrelfish: non VFS dependent stdin/stdout/stderr */
  s->_stdin->_read  = term_read;
  s->_stdin->_write = write_fail;
  s->_stdin->_seek  = seek_fail;
  s->_stdin->_close = close_fail; /* XXX ? */
  s->_stdout->_read  = read_fail;
  s->_stdout->_write = term_write;
  s->_stdout->_seek  = seek_fail;
  s->_stdout->_close = close_fail; /* XXX ? */
  s->_stderr->_read  = read_fail;
  s->_stderr->_write = term_write;
  s->_stderr->_seek  = seek_fail;
  s->_stderr->_close = close_fail; /* XXX ? */

  __sinit_lock_release ();
}

#ifndef __SINGLE_THREAD__

__LOCK_INIT_RECURSIVE(static, __sfp_lock);
__LOCK_INIT_RECURSIVE(static, __sinit_lock);

_VOID
_DEFUN_VOID(__sfp_lock_acquire)
{
  __lock_acquire_recursive (__sfp_lock);
}

_VOID
_DEFUN_VOID(__sfp_lock_release)
{
  __lock_release_recursive (__sfp_lock);
}

_VOID
_DEFUN_VOID(__sinit_lock_acquire)
{
  __lock_acquire_recursive (__sinit_lock);
}

_VOID
_DEFUN_VOID(__sinit_lock_release)
{
  __lock_release_recursive (__sinit_lock);
}

/* Walkable file locking routine.  */
static int
_DEFUN(__fp_lock, (ptr),
       FILE * ptr)
{
  _flockfile (ptr);

  return 0;
}

/* Walkable file unlocking routine.  */
static int
_DEFUN(__fp_unlock, (ptr),
       FILE * ptr)
{
  _funlockfile (ptr);

  return 0;
}

_VOID
_DEFUN_VOID(__fp_lock_all)
{
  __sfp_lock_acquire ();

  _CAST_VOID _fwalk (_REENT, __fp_lock);
}

_VOID
_DEFUN_VOID(__fp_unlock_all)
{
  _CAST_VOID _fwalk (_REENT, __fp_unlock);

  __sfp_lock_release ();
}
#endif
