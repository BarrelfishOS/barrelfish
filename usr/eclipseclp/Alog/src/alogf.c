#define ALOG_TRACE
#include "alog.h"


alogfmaster_(id,opt)		/* ALOG_MASTER */
int *id, *opt;
{
    /* printf("alogfm_: id=%d opt=%d\n",*id,*opt); */
    ALOG_MASTER(*id,*opt);
}

alogfsetup_(id,opt)		/* ALOG_SETUP */
int *id, *opt;
{
    /* printf("alogfs_: id=%d opt=%d\n",*id,*opt); */
    ALOG_SETUP(*id,*opt);
}

alogfdefine_(event,edef,fdef)	/* ALOG_DEFINE */
int *event;
char *edef, *fdef;
{
    /* printf("alogfd_: event=%d edef=%s fdef=%s\n",*event,edef,fdef); */
    ALOG_DEFINE(*event,edef,fdef);
}

alogflog_(id,etype,data,string)	/* ALOG_LOG */
int *id, *etype, *data;
char *string;
{
    /* printf("alogl_: id=%d etype=%d data=%d string=%s\n",*id,*etype,*data,string); */
    ALOG_LOG(*id,*etype,*data,string);
}

alogfoutput_()			/* ALOG_OUTPUT */
{
    /* printf("alogfo_: \n"); */
    ALOG_OUTPUT;
}

alogfstatus_(status)			/* ALOG_STATUS */
int *status;
{
    /* printf("alogft_: \n"); */
    ALOG_STATUS(*status);
}

alogfsetdir_(dir)			/* ALOG_SETDIR */
char *dir;
{
    /* printf("alogfr_: \n"); */
    ALOG_SETDIR(dir);
}

alogfenable_()                         /* ALOG_ENABLE */
{
    /* printf("alogf1_: \n"); */
    ALOG_ENABLE;
}

alogfdisable_()                        /* ALOG_DISABLE */
{
    /* printf("alogf1_: \n"); */
    ALOG_DISABLE;
}

