#include <stdio.h>
#include <string.h>

#include <eclipse.h>
#include <barrelfish/barrelfish.h>
#include <if/skb_defs.h>
#include <include/skb_server.h>
#include <include/skb_debug.h>

#include <dist2_server/service.h>
#include "predicates.h"

/*
int p_identification_complete(void)         // identification_complete(+Integer)
{
	long int id;
    ec_get_long(ec_arg(1), &id);

    struct dist_reply_state* drs = NULL;
    errval_t err = new_dist_reply_state(&drs, identification_complete_reply);
    assert(err_is_ok(err)); // TODO

    struct dist2_binding* b = (struct dist2_binding*) id;
    drs->rpc_reply(b, drs);

    return PSUCCEED;
}*/


int p_notify_client(void)         /* p_notify_client(+String, ReplyState) */
{
    printf("p_notify_client\n");

    struct dist_reply_state* drs;
    char* str = NULL;

    ec_get_string(ec_arg(1), &str); // TODO do we need to free this?
    ec_get_long(ec_arg(2), (long int*) &drs); // TODO conversion to pointer?
    assert(strlen(str)+1 < BUFFER_SIZE); // TODO

    strcpy(drs->query_state.stdout.buffer, str);
    debug_printf("p_notify_client: %s\n", drs->query_state.stdout.buffer);

    drs->error = SYS_ERR_OK;
    drs->reply(drs->binding, drs);

    return PSUCCEED;
}


int p_trigger_watch(void) /* p_trigger_watch(+String, +Mode, +Recipient, +WatchId, -Retract) */
{
    printf("p_trigger_watch\n");
    int res;

    // Get arguments
    char* record = NULL;
    res = ec_get_string(ec_arg(1), &record);
    if (res != PSUCCEED) {
        return res;
    }
    assert(strlen(record)+1 < BUFFER_SIZE); // TODO

    long int mode = 0;
    res = ec_get_long(ec_arg(2), &mode);
    if (res != PSUCCEED) {
        return res;
    }

    struct dist_reply_state* drs = NULL;
    res = ec_get_long(ec_arg(3), (long int*) &drs);
    if (res != PSUCCEED) {
        return res;
    }
    assert(drs != NULL);

    long int watch_id = 0;
    res = ec_get_long(ec_arg(4), &watch_id);
    if (res != PSUCCEED) {
        return res;
    }

    strcpy(drs->query_state.stdout.buffer, record);
    debug_printf("p_trigger_watch: %s\n", drs->query_state.stdout.buffer);

    drs->error = SYS_ERR_OK;
    drs->reply(drs->binding, drs);

    long int retract = true;
    return ec_unify_arg(5, ec_long(retract));
}

