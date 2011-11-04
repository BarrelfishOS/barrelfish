/**
 * \file
 */

#include "queue.h"

/**
 * \brief Enqueue on a waitset queue.
 *
 * \param qs    Pointer to queue to enqueue on
 * \param ms    Pointer to element to enqueue
 *
 * \return true if queue was empty, false if not.
 */
static bool enqueue_send(struct msg_queue *q, struct msg_queue_elem *m)
{
    assert(m->next == NULL);

    // Enqueue on the queue
    if(q->tail != NULL) {
        q->tail->next = m;
    } else {
        assert(q->head == NULL);
        q->head = m;
    }
    q->tail = m;

    return q->head == q->tail ? true : false;
}

/**
 * \brief Enqueue an element on a waitset queue IN FRONT.
 *
 * \param qs    Pointer to queue to enqueue on
 * \param ms    Pointer to element to enqueue
 *
 * \return true if queue was empty, false if not.
 */
static bool enqueue_send_at_front(struct msg_queue *q, struct msg_queue_elem *m)
{
    assert(m->next == NULL);
    if(q->tail == NULL) {
        assert(q->head == NULL);
    	q->head = m;
    	q->tail = m;
    } else {
    	m->next = q->head;
    	q->head = m;
    }
    return q->head == q->tail ? true : false;
}

static struct msg_queue_elem *dequeue_send(struct msg_queue *q)
{
    // Queue should have at least one element
    assert(q->head != NULL && q->tail != NULL);

    struct msg_queue_elem *e = q->head;
    q->head = e->next;
    if(q->head == NULL) {
        q->tail = NULL;
    }

    return e;
}

static void skb_send_handler(void *arg)
{
    struct skb_binding *b = arg;
    struct skb_queue_state *is = b->st;
    struct msg_queue *mq = &is->queue;

    // Dequeue next element from the queue
    struct skb_msg_queue_elem *e =
        (struct skb_msg_queue_elem *)dequeue_send(mq);

    // If the queue is non-empty, re-register
    if (!msg_queue_is_empty(mq)) {
        struct waitset *ws = get_default_waitset(); // XXX: store this on the q?
        errval_t err = b->register_send(b, ws, MKCONT(skb_send_handler,b));
        assert(err_is_ok(err));
    }

    assert(e->cont != NULL);
    e->cont(b, e);
}

errval_t skb_enqueue_send(struct skb_binding *b, struct msg_queue *q,
                               struct waitset *ws, struct msg_queue_elem *ms)
{
    ms->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue_send(q, ms)) {
        return b->register_send(b, ws, MKCONT(skb_send_handler,b));
    } else {
        return SYS_ERR_OK;
    }
}

errval_t skb_enqueue_send_at_front(struct skb_binding *b, struct msg_queue *q,
                                   struct waitset *ws, struct msg_queue_elem *ms)
{
    ms->next = NULL;

    // If queue was empty, enqueue on waitset
    if(enqueue_send_at_front(q, ms)) {
        return b->register_send(b, ws, MKCONT(skb_send_handler,b));
    } else {
        return SYS_ERR_OK;
    }
}
