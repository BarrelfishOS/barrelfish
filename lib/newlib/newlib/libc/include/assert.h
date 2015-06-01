/*
	assert.h
*/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _ASSERT_H_
#define _ASSERT_H_

#include "_ansi.h"

#undef assert

#ifdef NDEBUG           /* required by ANSI standard */
#define assert(__e)     ((void)sizeof(__e)) /* barrelfish assert */
/* # define assert(__e) ((void)0) */
#else
# define assert(__e) ((__e) ? (void)0 : __assert_func (__FILE__, __LINE__, \
						       __ASSERT_FUNC, #__e))

# ifndef __ASSERT_FUNC
  /* Use g++'s demangled names in C++.  */
#  if defined __cplusplus && defined __GNUC__
#   define __ASSERT_FUNC __PRETTY_FUNCTION__

  /* C99 requires the use of __func__.  */
#  elif __STDC_VERSION__ >= 199901L
#   define __ASSERT_FUNC __func__

  /* Older versions of gcc don't have __func__ but can use __FUNCTION__.  */
#  elif __GNUC__ >= 2
#   define __ASSERT_FUNC __FUNCTION__

  /* failed to detect __func__ support.  */
#  else
#   define __ASSERT_FUNC ((char *) 0)
#  endif
# endif /* !__ASSERT_FUNC */
#endif /* !NDEBUG */

void _EXFUN(__assert, (const char *, int, const char *)
	    _ATTRIBUTE ((__noreturn__)));
void _EXFUN(__assert_func, (const char *, int, const char *, const char *)
	    _ATTRIBUTE ((__noreturn__)));

#if __STDC_VERSION__ >= 201112L && !defined __cplusplus
# define static_assert _Static_assert
#endif

#endif /* _ASSERT_H_ */

#ifdef __cplusplus
}
#endif
