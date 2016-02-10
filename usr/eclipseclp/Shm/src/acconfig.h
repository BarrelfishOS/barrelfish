/* acconfig.h
   This file is in the public domain.

   Descriptive text for the C preprocessor macros that
   the distributed Autoconf macros can define.
   No software package will use all of them; autoheader copies the ones
   your configure.in uses into your configuration header file templates.

   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  Although this order
   can split up related entries, it makes it easier to check whether
   a given entry is in the file.

   Leave the following blank line there!!  Autoheader needs it.  */


/* PATH_MAX is declared in limits.h */
#undef PATH_IN_LIMITS

/* memcpy() is declared in string.h */
#undef MEMCPY_STRING

/* memcpy() is declared in memory.h */
#undef MEMCPY_MEMORY

/* sbrk() is not declared */
#undef SBRK_UNDEF

/* compiler doesn't understand void pointers */
#undef HAVE_NO_VOID_PTR


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
