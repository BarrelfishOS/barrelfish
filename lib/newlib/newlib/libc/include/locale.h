/*
	locale.h
	Values appropriate for the formatting of monetary and other
	numberic quantities.
*/

#ifndef _LOCALE_H_
#define _LOCALE_H_

#include "_ansi.h"

#define __need_NULL
#include <stddef.h>

#define LC_ALL	    0
#define LC_COLLATE  1
#define LC_CTYPE    2
#define LC_MONETARY 3
#define LC_NUMERIC  4
#define LC_TIME     5
#define LC_MESSAGES 6
#define LC_PAPER    7
#define LC_NAME     8
#define LC_ADDRESS  9
#define LC_TELEPHONE 10
#define LC_MEASUREMENT  11
#define LC_IDENTIFICATION 12

_BEGIN_STD_C

struct lconv
{
  char *decimal_point;
  char *thousands_sep;
  char *grouping;
  char *int_curr_symbol;
  char *currency_symbol;
  char *mon_decimal_point;
  char *mon_thousands_sep;
  char *mon_grouping;
  char *positive_sign;
  char *negative_sign;
  char int_frac_digits;
  char frac_digits;
  char p_cs_precedes;
  char p_sep_by_space;
  char n_cs_precedes;
  char n_sep_by_space;
  char p_sign_posn;
  char n_sign_posn;
  char int_n_cs_precedes;
  char int_n_sep_by_space;
  char int_n_sign_posn;
  char int_p_cs_precedes;
  char int_p_sep_by_space;
  char int_p_sign_posn;
};

#ifndef _REENT_ONLY
char *_EXFUN(setlocale,(int category, const char *locale));
struct lconv *_EXFUN(localeconv,(void));
#endif

struct _reent;
char *_EXFUN(_setlocale_r,(struct _reent *, int category, const char *locale));
struct lconv *_EXFUN(_localeconv_r,(struct _reent *));

// For POSIX compatibility
// From: The Open Group Base Specifications Issue 7, IEEE Std 1003.1-2008
/* Structure for reentrant locale using functions.  This is an
     (almost) opaque type for the user level programs.  The file and
     this data structure is not standardized.  Don't rely on it.  It can
     go away without warning.  */
typedef struct __locale_struct
{
    /* Note: LC_ALL is not a valid index into this array.  */
    struct __locale_data *__locales[13]; /* 13 = __LC_LAST. */

    /* To increase the speed of this solution we add some special members.  */
    const unsigned short int *__ctype_b;
    const int *__ctype_tolower;
    const int *__ctype_toupper;

    /* Note: LC_ALL is not a valid index into this array.  */
    const char *__names[13];
} *__locale_t;

/* POSIX 2008 makes locale_t official.  */
typedef __locale_t locale_t;

/* Return a reference to a data structure representing a set of locale
   datasets.  Unlike for the CATEGORY parameter for `setlocale' the
   CATEGORY_MASK parameter here uses a single bit for each category,
   made by OR'ing together LC_*_MASK bits above.  */
extern locale_t newlocale (int __category_mask, __const char *__locale,
                locale_t __base);

/* Return a duplicate of the set of locale in DATASET.  All usage
   counters are increased if necessary.  */
extern locale_t duplocale (locale_t __dataset);

/* Free the data associated with a locale dataset previously returned
   by a call to `setlocale_r'.  */
extern void freelocale (locale_t __dataset);

/* Switch the current thread's locale to DATASET.
   If DATASET is null, instead just return the current setting.
   The special value LC_GLOBAL_LOCALE is the initial setting
   for all threads and can also be installed any time, meaning
   the thread uses the global settings controlled by `setlocale'.  */
extern locale_t uselocale (locale_t __dataset);

/* This value can be passed to `uselocale' and may be returned by it.
   Passing this value to any other function has undefined behavior.  */
# define LC_GLOBAL_LOCALE   ((__locale_t) -1L)

/* These are the bits that can be set in the CATEGORY_MASK argument to
   `newlocale'.  In the GNU implementation, LC_FOO_MASK has the value
   of (1 << LC_FOO), but this is not a part of the interface that
   callers can assume will be true.  */
# define LC_CTYPE_MASK      (1 << LC_CTYPE)
# define LC_NUMERIC_MASK    (1 << LC_NUMERIC)
# define LC_TIME_MASK       (1 << LC_TIME)
# define LC_COLLATE_MASK    (1 << LC_COLLATE)
# define LC_MONETARY_MASK   (1 << LC_MONETARY)
# define LC_MESSAGES_MASK   (1 << LC_MESSAGES)
# define LC_PAPER_MASK      (1 << LC_PAPER)
# define LC_NAME_MASK       (1 << LC_NAME)
# define LC_ADDRESS_MASK    (1 << LC_ADDRESS)
# define LC_TELEPHONE_MASK  (1 << LC_TELEPHONE)
# define LC_MEASUREMENT_MASK    (1 << LC_MEASUREMENT)
# define LC_IDENTIFICATION_MASK (1 << LC_IDENTIFICATION)
# define LC_ALL_MASK        (LC_CTYPE_MASK \
                 | LC_NUMERIC_MASK \
                 | LC_TIME_MASK \
                 | LC_COLLATE_MASK \
                 | LC_MONETARY_MASK \
                 | LC_MESSAGES_MASK \
                 | LC_PAPER_MASK \
                 | LC_NAME_MASK \
                 | LC_ADDRESS_MASK \
                 | LC_TELEPHONE_MASK \
                 | LC_MEASUREMENT_MASK \
                 | LC_IDENTIFICATION_MASK \
                 )



_END_STD_C

#endif /* _LOCALE_H_ */
