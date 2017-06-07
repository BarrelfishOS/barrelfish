#ifndef NOTIFICATOR_H
#define NOTIFICATOR_H


#include <barrelfish/waitset.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>

typedef bool (*check_notification_fn_type)(void *object);

struct notificator
{
    struct notificator *prev, *next;
    void *object;
    check_notification_fn_type can_read, can_write;
    struct waitset_chanstate ready_to_read, ready_to_write;
};


void notificator_init(struct notificator *notificator, void *object,
    check_notification_fn_type can_read, check_notification_fn_type can_write);

void check_notificators_disabled(dispatcher_handle_t handle);

#endif
