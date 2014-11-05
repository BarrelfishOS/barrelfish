#include <if/intermon_defs.h>
#include <capops.h>
#include "monitor_debug.h"
#include "internal.h"
#include "delete_int.h"

errval_t capops_init(struct waitset *ws, struct intermon_binding *b)
{
    DEBUG_CAPOPS("%s\n", __FUNCTION__);

    assert(ws != NULL);

    b->rx_vtbl.capops_request_copy            = request_copy__rx;
    b->rx_vtbl.capops_recv_copy               = recv_copy__rx;
    b->rx_vtbl.capops_recv_copy_result        = recv_copy_result__rx;
    b->rx_vtbl.capops_move_request            = move_request__rx_handler;
    b->rx_vtbl.capops_move_result             = move_result__rx_handler;
    b->rx_vtbl.capops_retrieve_request        = retrieve_request__rx;
    b->rx_vtbl.capops_retrieve_result         = retrieve_result__rx;
    b->rx_vtbl.capops_delete_remote           = delete_remote__rx;
    b->rx_vtbl.capops_delete_remote_result    = delete_remote_result__rx;
    b->rx_vtbl.capops_revoke_mark             = revoke_mark__rx;
    b->rx_vtbl.capops_revoke_ready            = revoke_ready__rx;
    b->rx_vtbl.capops_revoke_commit           = revoke_commit__rx;
    b->rx_vtbl.capops_revoke_done             = revoke_done__rx;
    b->rx_vtbl.capops_request_retype          = retype_request__rx;
    b->rx_vtbl.capops_retype_response         = retype_response__rx;
    b->rx_vtbl.capops_update_owner            = update_owner__rx_handler;
    b->rx_vtbl.capops_owner_updated           = owner_updated__rx_handler;
    b->rx_vtbl.capops_find_cap                = find_cap__rx_handler;
    b->rx_vtbl.capops_find_cap_result         = find_cap_result__rx_handler;
    b->rx_vtbl.capops_find_descendants        = find_descendants__rx_handler;
    b->rx_vtbl.capops_find_descendants_result = find_descendants_result__rx_handler;

    delete_steps_init(ws);

    return SYS_ERR_OK;
}
