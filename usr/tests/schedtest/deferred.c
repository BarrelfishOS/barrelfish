#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

static void defhandle(void *dummy)
{
    printf("event\n");
}

int main(int argc, char *argv[])
{
    static struct deferred_event de;

    deferred_event_init(&de);
    errval_t err = deferred_event_register(&de, get_default_waitset(), 15000000,
                                           MKCLOSURE(defhandle, NULL));
    assert(err_is_ok(err));

    /* err = event_dispatch(get_default_waitset()); */
    /* assert(err_is_ok(err)); */

    thread_yield_dispatcher(NULL_CAP);

    return 0;
}
