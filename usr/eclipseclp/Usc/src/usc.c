/**********************************************************************
** Copyright (c) 1995 European Computer-Industry Research Centre GmbH
** All rights reserved.
***********************************************************************
**  System: USC - Micro Second Clock
**    File: usc.c
** Authors: Kees Schuerman
**  SccsId: "@(#)usc.c	1.6 3/8/95"
***********************************************************************/

#include "machine.h"


/**********************************************************************
** GETHRTIME()
***********************************************************************/

#if (defined(SUNOS5) 		&& \
     defined(HAVE_GETHRTIME) 	&& \
     defined(HAVE_SYS_TIME_H))

#include <sys/time.h>

#include "usc.h"


static usc_time_t usc_MD_rollover_val = 0;

usc_time_t usc_rollover_val = 0;


void usc_init()
{
    usc_time_t roll;

    roll = (usc_time_t) 
	   ((usc_time_t) 1 << ((sizeof(usc_time_t)*8)-1));
    roll = roll + roll - 1;
    usc_MD_rollover_val = (usc_time_t) (roll / 1000000);
    usc_rollover_val = usc_MD_rollover_val * 1000000 - 1;
}


usc_time_t usc_clock()
{
    unsigned long ustime;
    unsigned long clock_sec;
    unsigned long clock_usec;
    hrtime_t hrtime;

    hrtime = gethrtime();
    clock_sec = (unsigned long) (hrtime  / 1000000000);
    clock_usec = (unsigned long) 
		 (hrtime / 1000 - 1000000 * (hrtime_t) clock_sec);
    ustime = clock_sec % usc_MD_rollover_val;
    ustime = (ustime * 1000000) + clock_usec;

    return((usc_time_t) ustime);
}

#else


/**********************************************************************
** GETTIMEOFDAY()
***********************************************************************/

#if (defined(HAVE_GETTIMEOFDAY) && \
     defined(HAVE_SYS_TIME_H))

#include <sys/time.h>

#include "usc.h"

static usc_time_t usc_MD_rollover_val = 0;

usc_time_t usc_rollover_val = 0;


void usc_init()
{
    usc_time_t roll;

    roll = (usc_time_t) 
	   ((usc_time_t) 1 << ((sizeof(usc_time_t)*8)-1));
    roll = roll + roll - 1;
    usc_MD_rollover_val = (usc_time_t) (roll / 1000000);
    usc_rollover_val = usc_MD_rollover_val * 1000000 - 1;
}


usc_time_t usc_clock()
{
    unsigned long ustime;
    struct timeval tp;

#if !defined(SVR4)
    struct timezone tzp;
    gettimeofday(&tp,&tzp);
#else
    gettimeofday(&tp);
#endif

    ustime = (unsigned long) tp.tv_sec;
    ustime = ustime % usc_MD_rollover_val;
    ustime = (ustime * 1000000) + (unsigned long) tp.tv_usec;

    return((usc_time_t) ustime);
}

#else


/**********************************************************************
** TIMES()
***********************************************************************/

#if (defined(SVR4) 		&& \
     defined(HAVE_TIMES) 	&& \
     defined(HAVE_UNISTD_H) 	&& \
     defined(HAVE_SYS_TYPES_H)	&& \
     defined(HAVE_SYS_TIME_H))

#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>

#include "usc.h"


static usc_time_t usc_MD_rollover_val = 0;
static long clk_tck;

usc_time_t usc_rollover_val = 0;


void usc_init()
{
    usc_time_t roll;

    clk_tck = sysconf(_SC_CLK_TCK); 

    roll = (usc_time_t) 
	   ((usc_time_t) 1 << ((sizeof(usc_time_t)*8)-1));
    roll = roll + roll - 1;
    usc_MD_rollover_val = (usc_time_t) (roll / 1000000);
    usc_rollover_val = usc_MD_rollover_val * 1000000 - 1;
}


usc_time_t usc_clock()
{
    
    unsigned long ustime;
    unsigned long clock_sec;
    unsigned long clock_usec;
    struct tms buffer;
    clock_t clock;

    clock = times(&buffer);
    if (clock == (clock_t) -1)
	return((usc_time_t) 0);

    clock_sec = (unsigned long) (clock / clk_tck);
    clock_usec = (clock - clock_sec * clk_tck) * 1000000 / clk_tck;
    ustime = clock_sec % usc_MD_rollover_val;
    ustime = (ustime * 1000000) + clock_usec;

    return((usc_time_t) ustime);
}


#else

/**********************************************************************
** NOP()
***********************************************************************/

void usc_init()
{
    return;
}

usc_time_t usc_clock()
{
    return((usc_time_t) 0);
}

#endif /* HAVE_TIMES */
#endif /* HAVE_GETTIMEOFDAY */
#endif /* HAVE_GETHRTIME */

