#include <barrelfish/notificator.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/waitset_chan.h>
#include <include/waitset_chan_priv.h>

/// Dequeue the element from the notificator queue
// static void dequeue(struct notificator **queue,
//                             struct notificator *element)
// {
//     if (element->next == element) {
//         assert(element->prev == element);
//         assert(*queue == element);
//         *queue = NULL;
//     } else {
//         element->prev->next = element->next;
//         element->next->prev = element->prev;
//         if (*queue == element) {
//             *queue = element->next;
//         }
//     }
//     element->prev = element->next = NULL;
// }

/// Enqueue the element on the notificator queue
static void enqueue(struct notificator **queue,
                            struct notificator *element)
{
    if (*queue == NULL) {
        *queue = element;
        element->next = element->prev = element;
    } else {
        element->next = *queue;
        element->prev = (*queue)->prev;
        element->next->prev = element;
        element->prev->next = element;
    }
}

void notificator_init(struct notificator *notificator, void *object,
    check_notification_fn_type can_read, check_notification_fn_type can_write)
{
    notificator->prev = NULL;
    notificator->next = NULL;
    notificator->object = object;
    notificator->can_read = can_read;
    notificator->can_write = can_write;

    waitset_chanstate_init(&notificator->ready_to_read, CHANTYPE_OTHER);
    notificator->ready_to_read.persistent = true;
    waitset_chanstate_init(&notificator->ready_to_write, CHANTYPE_OTHER);
    notificator->ready_to_write.persistent = true;

    dispatcher_handle_t handle = disp_disable();
    enqueue(&get_dispatcher_generic(handle)->notificators, notificator);
    disp_enable(handle);
}

void check_notificators_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    struct notificator *n;

    if (!dp->notificators)
        return;
    n = dp->notificators;
    do {
        if (n->can_read(n->object)) {
            if (waitset_chan_is_registered(&n->ready_to_read)) {
                // debug_printf("triggering\n");
                errval_t err = waitset_chan_trigger_disabled(&n->ready_to_read, handle);
                assert(err_is_ok(err)); // should not fail
            }
        }
        if (n->can_write(n->object)) {
            if (waitset_chan_is_registered(&n->ready_to_write)) {
                // debug_printf("triggering\n");
                errval_t err = waitset_chan_trigger_disabled(&n->ready_to_write, handle);
                assert(err_is_ok(err)); // should not fail
            }
        }
        n = n->next;
    } while (n != dp->notificators);
}
