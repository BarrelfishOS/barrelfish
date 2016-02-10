/**********************************************************************
**           System: ALOG
**             File: usc.h
**           Author: Arun Nanda
**		   : Kees Schuerman
**           SccsId: "@(#)alog.h	1.1 11/4/94"
**      Description: ALOG Interface
***********************************************************************/

#ifndef _ALOG_H_
#define _ALOG_H_

#include <stdio.h>
#include "usc.h"


/*
** Constants 
*/

#define MAX_DIRNAME_LEN 	100
#define MAX_LOG_STRING_LEN 	12
#define MAX_BUF_SIZE       	100
#define ALOG_LOGFILE		"alogfile.p"

#define ALOG_TRUNCATE		0
#define ALOG_WRAP		1

#define ALOG_OFF		0
#define ALOG_ON			1

#define ALOG_EVENT_SYNC        -101
#define ALOG_EVENT_PAIR_A1     -102
#define ALOG_EVENT_PAIR_A2     -103
#define ALOG_EVENT_PAIR_B1     -104


/*
** Structure Definitions
*/

struct head_trace_buf {
        int             next_entry;
        int             max_size;
        unsigned long   prev_time;
        unsigned long   ind_time;
        int             trace_flag;
        struct trace_buf *xx_list;
        struct trace_buf *cbuf;
        FILE            *file_t;
};

struct trace_buf {
        struct trace_buf *next_buf;
        struct trace_table {
                int     id;
                int     task_id;
                int     event;
                int     data_int;
                char    data_string[MAX_LOG_STRING_LEN+1];
                unsigned long     tind;
                unsigned long     tstamp;
        } ALOG_table[MAX_BUF_SIZE];
};


/*
** Variables
*/

extern int xx_alog_status;
extern int xx_alog_setup_called;
extern int xx_alog_output_called;
extern char xx_alog_outdir[];
extern struct head_trace_buf *xx_buf_head;


/*
** Functions
*/

#if defined(__STDC__)
void xx_write(struct head_trace_buf * head,
	      int pid, int event, int data1, char * data2);
void xx_dump(struct head_trace_buf * head); 
void xx_dump_aux(struct trace_buf * buf,
		 FILE * fp, int xx_j, int xx_k);
void xx_user(struct head_trace_buf * head, int id);
void xx_user1(struct head_trace_buf * head, int id);
void xx_alog_setup(int pid, int flag);
int  xx_getbuf(struct head_trace_buf * head);
#else /* __STDC__ */
void xx_write();
void xx_dump(); 
void xx_dump_aux();
void xx_user();
void xx_user1();
void xx_alog_setup();
int  xx_getbuf();
#endif /* __STDC__ */


/*
** Macro's
*/

#ifdef ALOG_TRACE

#define ALOG_DEC

#define ALOG_STATUS(status) \
	if ((status) == ALOG_ON) \
		xx_alog_status |= 0x1; \
	else \
		xx_alog_status &= ~0x1

#define ALOG_ENABLE ALOG_STATUS(ALOG_ON)

#define ALOG_DISABLE ALOG_STATUS(ALOG_OFF)

#define ALOG_SETDIR(dir) \
	{\
	strncpy(xx_alog_outdir,(dir),MAX_DIRNAME_LEN); \
	xx_alog_outdir[MAX_DIRNAME_LEN] = '\0'; \
	}


#define ALOG_SETUP(pid,flag) \
        {\
            if (xx_alog_status & 0x1 &&  !xx_alog_setup_called) \
	    {\
                xx_alog_setup_called = 1;\
                xx_alog_setup((pid),(flag));\
	    }\
        }

#define ALOG_MASTER(pid,flag) \
	{\
	    if (xx_alog_status & 0x1) \
	    {\
	        xx_alog_setup((pid),(flag)); \
	        xx_user1(xx_buf_head,(pid)); \
	    }\
	}

#define ALOG_DEFINE(event,edef,strdef) \
        {\
        if (xx_alog_status & 0x1) \
        {\
            xx_write(xx_buf_head,0,(-9),(event),(edef)); \
            xx_write(xx_buf_head,0,(-10),(event),(strdef)); \
        }\
        }

#define ALOG_LOG(pid,type,data1,data2) \
	{\
	if (xx_alog_status & 0x1) \
		xx_write(xx_buf_head,(pid),(type),(data1),(data2)); \
	}

#define ALOG_OUTPUT \
        {\
            if (xx_alog_status & 0x1  &&  !xx_alog_output_called) \
	    {\
                xx_alog_output_called = 1;\
                xx_dump(xx_buf_head);\
	    }\
        }

#else /* ALOG_TRACE */

#define ALOG_DEC 
#define ALOG_STATUS(a)
#define ALOG_ENABLE
#define ALOG_DISABLE
#define ALOG_SETDIR(a)
#define ALOG_SETUP(a,b)
#define ALOG_MASTER(a,b)
#define ALOG_DEFINE(a,b,c)
#define ALOG_LOG(a,b,c,d)
#define ALOG_OUTPUT

#endif /* ALOG_TRACE */


#endif /* _ALOG_H_ */
