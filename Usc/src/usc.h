/**********************************************************************
**           System: USC - Micro Second Clock    
**             File: usc.h
**          Authors: Kees Schuerman
**           SccsId: "@(#)usc.h	1.3 1/20/95"
**      Description: USC Interface
***********************************************************************/

#ifndef _USC_H_
#define _USC_H_

/*
** Type Definitions
*/

typedef unsigned long usc_time_t;


/*
** Global Variables
*/

extern usc_time_t usc_rollover_val;


/*
** Clock Primitives
*/

#if defined(__STDC__)
extern void usc_init(void);
extern usc_time_t usc_clock(void);
#else /* __STDC__ */
extern void usc_init();
extern usc_time_t usc_clock();
#endif /* __STDC__ */

#endif /* _USC_H_ */

