/*-
 * Copyright (c) 2010 Isilon Systems, Inc.
 * Copyright (c) 2010 iX Systems, Inc.
 * Copyright (c) 2010 Panasas, Inc.
 * Copyright (c) 2013-2015 Mellanox Technologies, Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice unmodified, this list of conditions, and the following
 *    disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef	_LINUX_WORKQUEUE_H_
#define	_LINUX_WORKQUEUE_H_

/*#include <linux/types.h>
 #include <linux/kernel.h>
 #include <linux/timer.h>
 #include <linux/slab.h>

 #include <asm/atomic.h>

 #include <sys/taskqueue.h>
 */
#include <linux/list.h>

#include <barrelfish/threads.h>

struct workqueue_struct {
	void *name;
	struct thread_cond cond;
	struct thread *thread;
	struct thread_mutex work_list_lock;
	struct list_head work_list;
};

struct work_struct {
	struct list_head list;
	void (*fn)(struct work_struct *);
};

static int enqueue(struct workqueue_struct *wq, struct work_struct *w) {
	thread_mutex_lock(&wq->work_list_lock);
	list_add_tail(&w->list, &wq->work_list);
	thread_mutex_unlock(&wq->work_list_lock);

	thread_cond_signal(&wq->cond);
	return 0;
}

static struct work_struct *dequeue(struct workqueue_struct *wq) {
	struct work_struct *w = NULL;
	if (list_empty(&wq->work_list)) {
		thread_cond_wait(&wq->cond, NULL);
	} else {
		thread_mutex_lock(&wq->work_list_lock);
		w = list_entry(wq->work_list.next, struct work_struct, list);
		list_del(&w->list);
		thread_mutex_unlock(&wq->work_list_lock);
	}
	return w;
}

static int exec_work(void *wq) {
	struct work_struct *w;
	for (;;) {
		w = dequeue(wq);
		if (w)
			w->fn(w);
	}
	return 0;
}

/*
 typedef __typeof(((struct work_struct *)0)->fn) work_func_t;

 struct delayed_work {
 struct work_struct	work;
 struct callout		timer;
 };

 extern void linux_work_fn(void *, int);
 extern void linux_flush_fn(void *, int);
 extern void linux_delayed_work_fn(void *);
 extern struct workqueue_struct *linux_create_workqueue_common(const char *, int);
 extern void destroy_workqueue(struct workqueue_struct *);

 static inline struct delayed_work *
 to_delayed_work(struct work_struct *work)
 {

 return container_of(work, struct delayed_work, work);
 }
 */

#define	INIT_WORK(work, func) 	 					\
do {									\
	(work)->fn = (func);						\
	INIT_LIST_HEAD(&(work)->list);		\
} while (0)

/*
 #define	INIT_DELAYED_WORK(_work, func)					\
do {									\
	INIT_WORK(&(_work)->work, func);				\
	callout_init(&(_work)->timer, CALLOUT_MPSAFE);			\
} while (0)

 #define	INIT_DEFERRABLE_WORK(...) INIT_DELAYED_WORK(__VA_ARGS__)

 #define	schedule_work(work)						\
do {									\
	(work)->taskqueue = taskqueue_thread;				\
	taskqueue_enqueue(taskqueue_thread, &(work)->work_task);	\
} while (0)

 #define	flush_scheduled_work()	flush_taskqueue(taskqueue_thread)
 */

static inline int queue_work(struct workqueue_struct *wq,
		struct work_struct *work) {
	return enqueue(wq, work);
}

/*
 static inline int
 queue_delayed_work(struct workqueue_struct *wq, struct delayed_work *work,
 unsigned long delay)
 {
 int pending;

 work->work.taskqueue = wq->taskqueue;
 if (atomic_read(&wq->draining) != 0) {
 pending = work->work.work_task.ta_pending;
 } else if (delay != 0) {
 pending = work->work.work_task.ta_pending;
 callout_reset(&work->timer, delay, linux_delayed_work_fn, work);
 } else {
 callout_stop(&work->timer);
 pending = taskqueue_enqueue(work->work.taskqueue,
 &work->work.work_task);
 }
 return (!pending);
 }

 static inline bool
 schedule_delayed_work(struct delayed_work *dwork,
 unsigned long delay)
 {
 struct workqueue_struct wq;

 wq.taskqueue = taskqueue_thread;
 atomic_set(&wq.draining, 0);
 return (queue_delayed_work(&wq, dwork, delay));
 }

 #define	create_singlethread_workqueue(name)				\
	linux_create_workqueue_common(name, 1)
 */

static inline struct workqueue_struct *create_singlethread_workqueue(void *name) {
	struct thread *thread = NULL;
	struct workqueue_struct *wq;

	wq = malloc(sizeof *wq);
	if (!wq)
		printf("Error while creating workqueue_struct");

	wq->name = name;
	wq->thread = thread;
	thread_cond_init(&wq->cond);
	INIT_LIST_HEAD(&wq->work_list);
	thread_mutex_init(&wq->work_list_lock);

	thread = thread_create(exec_work, wq);
	if (!thread)
		printf("Error while creating the working thread");

	return wq;
}

/*
 #define	create_workqueue(name)						\
	linux_create_workqueue_common(name, MAXCPU)

 #define	alloc_ordered_workqueue(name, flags)				\
	linux_create_workqueue_common(name, 1)

 #define	alloc_workqueue(name, flags, max_active)			\
	linux_create_workqueue_common(name, max_active)

 #define	flush_workqueue(wq)	flush_taskqueue((wq)->taskqueue)

 static inline void
 flush_taskqueue(struct taskqueue *tq)
 {
 struct task flushtask;

 PHOLD(curproc);
 TASK_INIT(&flushtask, 0, linux_flush_fn, NULL);
 taskqueue_enqueue(tq, &flushtask);
 taskqueue_drain(tq, &flushtask);
 PRELE(curproc);
 }

 static inline void
 drain_workqueue(struct workqueue_struct *wq)
 {
 atomic_inc(&wq->draining);
 flush_taskqueue(wq->taskqueue);
 atomic_dec(&wq->draining);
 }

 static inline int
 cancel_work_sync(struct work_struct *work)
 {
 if (work->taskqueue &&
 taskqueue_cancel(work->taskqueue, &work->work_task, NULL))
 taskqueue_drain(work->taskqueue, &work->work_task);
 return 0;
 }


 * This may leave work running on another CPU as it does on Linux.

 static inline int
 cancel_delayed_work(struct delayed_work *work)
 {

 callout_stop(&work->timer);
 if (work->work.taskqueue)
 return (taskqueue_cancel(work->work.taskqueue,
 &work->work.work_task, NULL) == 0);
 return 0;
 }

 static inline int
 cancel_delayed_work_sync(struct delayed_work *work)
 {

 callout_drain(&work->timer);
 if (work->work.taskqueue &&
 taskqueue_cancel(work->work.taskqueue, &work->work.work_task, NULL))
 taskqueue_drain(work->work.taskqueue, &work->work.work_task);
 return 0;
 }

 static inline bool
 mod_delayed_work(struct workqueue_struct *wq, struct delayed_work *dwork,
 unsigned long delay)
 {
 cancel_delayed_work(dwork);
 queue_delayed_work(wq, dwork, delay);
 return false;
 }*/

#endif	/* _LINUX_WORKQUEUE_H_ */
