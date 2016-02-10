/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * The template header file. We cannot use autoheader because fgrep
 * complains about too many symbols in configure.in
 */


/* This ECLiPSe version */
#undef PACKAGE_VERSION

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
#undef _ALL_SOURCE
#endif

/* Define if atof() might not return negative zeros correctly  */
#undef ATOF_NEGZERO_BUG

/* Define if type char is unsigned and you are not using gcc.  */
#undef __CHAR_UNSIGNED__

/* Define to empty if the keyword does not work.  */
#undef const

/* Define to empty if the keyword does not work.  */
#undef volatile

/* Define to empty if the keyword does not work.  */
#undef inline

/* ignored if not available */
#undef finite

/* Define if system calls automatically restart after interruption
   by a signal.  */
#undef HAVE_RESTARTABLE_SYSCALLS

/* Define if your struct stat has st_blksize.  */
#undef HAVE_ST_BLKSIZE

/* Define if you have vfork.h.  */
#undef HAVE_VFORK_H

/* Type sizes.  */
#undef SIZEOF_INT
#undef SIZEOF_LONG
#undef SIZEOF_CHAR_P
#undef SIZEOF_LONG_P

/* Define if no (void *) type.  */
#undef HAVE_NO_VOID_PTR

/* Define if we have the long long int type.  */
#undef HAVE_LONG_LONG
#undef HAVE___INT64

/* Define to `long' if <sys/types.h> doesn't define.  */
#undef off_t

/* Define to `int' if <sys/types.h> doesn't define.  */
#undef pid_t

/* Define as the return type of signal handlers (int or void).  */
#undef RETSIGTYPE

/* Define if sprintf() returns the number of characters printed. */
#undef SPRINTF_RETURNS_LENGTH

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
#undef size_t

/* Define if you have the ANSI C header files.  */
#undef STDC_HEADERS

/* Define on System V Release 4.  */
#undef SVR4

/* Define vfork as fork if vfork does not work.  */
#undef vfork

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
#undef WORDS_BIGENDIAN

/* Check where R_OK etc. are defined, unistd.h or sys/file.h */
#undef ACCESS_IN_UNISTD

/* On AIX/rs6000 define _BSD to obtain BSD-like features */
#undef _BSD

/* Check if times(3) returns 0 or elapsed time */
#undef BSD_TIMES

/* The right spelling for etext */
#define ETEXT undef

/* Check if the signal action is reset to SIG_DFL before calling the handler */
#undef HANDLER_RESET

/* Look for the alarm function */
#undef HAVE_ALARM

/* Check if cputime can be limited */
#undef HAVE_CPU_LIMIT

/* Check for the finite function */
#undef HAVE_FINITE

/* Check for the isinf and isnan functions */
#undef HAVE_ISINF
#undef HAVE_ISNAN

/* Look for the pipes */
#undef HAVE_PIPE

/* Check if it is possible to push back characters with TIOCSTI */
#undef HAVE_PUSHBACK

/* Look for the putenv function */
#undef HAVE_PUTENV

/* Look for the rename function */
#undef HAVE_RENAME

/* Look for the select function */
#undef HAVE_SELECT

/* Look for the setitimer function */
#undef HAVE_SETITIMER

/* Look for the sleep function */
#undef HAVE_SLEEP

/* Look for the strerror function */
#undef HAVE_STRERROR

/* Look for the localtime_r function */
#undef HAVE_LOCALTIME_R

/* Check for vsprintf nad if it is there, assume varargs is available */
#undef HAVE_VARARGS
#undef HAVE_VSPRINTF
#undef HAVE_VSNPRINTF

/* Check for ways to control floating point rounding */
#undef HAVE_FENV_H
#undef HAVE_FPU_CONTROL_H
#undef HAVE_FPSETROUND
#undef HAVE_IEEE_FLAGS

/* Specify the host architecture as returned by ARCH */
#define HOSTARCH unknown

/* Look for the PATH_MAX definition in <limits.h> */
#undef PATH_IN_LIMITS

/* Are the tests running on a remote machine? */
#undef REMOTE_TESTS

/* Define if sigio is available */
#undef SIGIO_FASYNC
#undef SIGIO_SETSIG

/* Check if sys/socket.h exists and if so, assume that sockets are there */
#undef SOCKETS

/* Check if sys/un.h exists and if so, assume the AF_UNIX address family
 * works
 */
#undef HAVE_AF_UNIX

/* Check how to obtain page size from sysconf() */
#undef SYSCONF_PAGE

/* Check for System V-style termio */
#undef TERMIO_SYS_V_STYLE

/* Define if you have dirent.h.  */
#undef HAVE_DIRENT_H

/* Define if you have fstat.  */
#undef HAVE_FSTAT

/* Define if you have getcwd.  */
#undef HAVE_GETCWD

/* Define if you have gethostid.  */
#undef HAVE_GETHOSTID

/* Define if you have gethostname.  */
#undef HAVE_GETHOSTNAME

/* Define if you have gethrtime (high resolution timer).  */
#undef HAVE_GETHRTIME

/* Define if you don't have dirent.h, but have ndir.h.  */
#undef HAVE_NDIR_H

/* Define if you have netdb.h.  */
#undef HAVE_NETDB_H

/* Define if you have sgi hardware high resolution timer.  */
#undef HAVE_SGIHRCLOCK

/* Define if sgi hardware high resolution timer is 64 bits  */
#undef HAVE_SGI64BITCLOCK

/* Define if you have getwd.  */
#undef HAVE_GETWD

/* Define if you have bcopy.  */
#undef HAVE_BCOPY

/* Define if you have memcpy.  */
#undef HAVE_MEMCPY

/* Define if you have memmove.  */
#undef HAVE_MEMMOVE

/* Define if you have a working mmap.  */
#undef HAVE_MMAP

/* Define if you have sys/mman.h.  */
#undef HAVE_SYS_MMAN_H

/* Define if you have random.  */
#undef HAVE_RANDOM

/* Define if you have realpath.  */
#undef HAVE_REALPATH

/* Define if you have rint.  */
#undef HAVE_RINT

/* Define if you have trunc.  */
#undef HAVE_TRUNC

/* Define if you have setsid.  */
#undef HAVE_SETSID

/* Define if you have sigaction.  */
#undef HAVE_SIGACTION

/* Define if you have sigaltstack.  */
#undef HAVE_SIGALTSTACK

/* Define if you have siginterrupt.  */
#undef HAVE_SIGINTERRUPT

/* Define if you have sigprocmask.  */
#undef HAVE_SIGPROCMASK

/* Define if you have sigstack.  */
#undef HAVE_SIGSTACK

/* Define if you have sigvec.  */
#undef HAVE_SIGVEC

/* Define if you have sincos(). Used in ria.  */
#undef HAVE_SINCOS

/* Define if you don't have dirent.h, but have sys/dir.h.  */
#undef HAVE_SYS_DIR_H

/* Define if you don't have dirent.h, but have sys/ndir.h.  */
#undef HAVE_SYS_NDIR_H

/* Define if you have sysconf.  */
#undef HAVE_SYSCONF

/* Define if you have sysinfo.  */
#undef HAVE_SYSINFO

/* Define if you have sys/systeminfo.h  */
#undef HAVE_SYS_SYSTEMINFO_H

/* Define if ioctl undestands SIOCGIFHWADDR */
#undef HAVE_SIOCGIFHWADDR

/* Define if you have tcgetattr.  */
#undef HAVE_TCGETATTR

/* Define if you have times.  */
#undef HAVE_TIMES

/* Define if we have <ucontext.h> and can acces registers from signal context.  */
#undef HAVE_UCONTEXTGREGS

/* Define if you have the <ctype.h> header file.  */
#undef HAVE_CTYPE_H

/* Define if you have the <memory.h> header file.  */
#undef HAVE_MEMORY_H

/* Define if you have the <pthread.h> header file.  */
#undef HAVE_PTHREAD_H

/* Define if you have the <string.h> header file.  */
#undef HAVE_STRING_H

/* Define if you have the <sys/param.h> header file.  */
#undef HAVE_SYS_PARAM_H

/* Define if you have the <sys/utsname.h> header file.  */
#undef HAVE_SYS_UTSNAME_H

/* Define if you have the <unistd.h> header file.  */
#undef HAVE_UNISTD_H

/* Define if you have the bsd library (-lbsd).  */
#undef HAVE_LIBBSD

/* Define if you have the m library (-lm).  */
#undef HAVE_LIBM

/* Define is getpagesize() exists */
#undef HAVE_GETPAGESIZE

/* On some machines, there is not enough space to run bigclause etc. */
#undef SMALL_SPACE

/* On some machines struct nlist uses n_un.n_name, on others it does not */
#undef N_NAME

/* Do we have the GMP (or compatible) multi precision library */
#undef HAVE_LIBGMP

/* Are we using MPIR as the GMP compatible multi precision library */
#undef USING_MPIR

/* Do we have the FLEXlm licence manager */
#undef HAVE_FLEXLM

/* Define if ceil() doesn't correctly return -0.0 for argument >-1.0 <-0.0 */
#undef HAVE_CEIL_NEGZERO_BUG                                                    
/* Define if rint() doesn't behave correctly */
#undef HAVE_RINT_BUG                                                 

/* Define if pow() gives incorrect results for 0.0^negint */
#undef HAVE_POW_ZERO_NEG_BUG                                                    
/* Define if compiler may be generating code with strict-overflow optmisation
   that breaks some code that does integer overflow detection */
#undef MAY_HAVE_STRICT_OVERFLOW

/*#define O o */
#undef OBJECT_SUFFIX_STRING
#undef AIX
#undef HAVE_MPROTECT
#undef HAVE_GETRUSAGE
#undef UPTIME
#undef HAVE_DLOPEN
#undef HAVE_MACH_O_DYLD_H
#undef HAVE_NLIST

#undef HAVE_TCL
#undef HAVE_TK
#undef SBRK_UNDEF
#undef GETHOSTID_UNDEF
#undef STRTOL_UNDEF
/* Define if we cannot compute the stack beginning from the sbrk(0) address */
#undef STACK_BASE

#define KB	1024
#define MB	KB*KB

#undef VIRTUAL_HEAP_DEFAULT
#undef VIRTUAL_SHARED_DEFAULT
#undef VIRTUAL_LOCAL_STACK_DEFAULT
#undef VIRTUAL_GLOBAL_STACK_DEFAULT
#undef SHARED_MEM_OFFSET_HEAP
#undef MEMCPY_STRING
#undef MEMCPY_MEMORY
#undef HAVE_SYS_SELECT_H
#undef HAVE_VALUES_H
#undef HAVE_INFINITY

/* on m88k, gettimeofday accepts only one argument */
#ifdef GETTIME1
#define gettimeofday(tp,tzp)	gettimeofday(tp)
#endif

/* define if compiling for Windows -- specifies minimum version */
#undef HAVE_WIN32_WINNT

/* to deal with OS's that define it but don't implement it */
#undef HAVE_MAP_NORESERVE

#define	NREGARG		0
#define	NREGTMP		0

