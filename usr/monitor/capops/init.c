#include <if/intermon_defs.h>
#include <capops.h>
#include "internal.h"

errval_t capops_intermon_init(struct intermon_binding *b)
{
    b->rx_vtbl.capops_request_copy            = request_copy__rx;
    b->rx_vtbl.capops_recv_copy               = recv_copy__rx;
    b->rx_vtbl.capops_recv_copy_result        = recv_copy_result__rx;
    b->rx_vtbl.capops_move_request            = move_request__rx_handler;
    b->rx_vtbl.capops_move_result             = move_result__rx_handler;
    b->rx_vtbl.capops_delete_remote           = delete_remote__rx_handler;
    b->rx_vtbl.capops_delete_remote_result    = delete_remote_result__rx_handler;
    b->rx_vtbl.capops_request_revoke          = request_revoke__rx_handler;
    b->rx_vtbl.capops_revoke_result           = revoke_result__rx_handler;
    b->rx_vtbl.capops_request_retype          = retype_request__rx;
    b->rx_vtbl.capops_retype_response         = retype_response__rx;
    b->rx_vtbl.capops_update_owner            = update_owner__rx_handler;
    b->rx_vtbl.capops_owner_updated           = owner_updated__rx_handler;
    b->rx_vtbl.capops_find_cap                = find_cap__rx_handler;
    b->rx_vtbl.capops_find_cap_result         = find_cap_result__rx_handler;
    b->rx_vtbl.capops_find_descendants        = find_descendants__rx_handler;
    b->rx_vtbl.capops_find_descendants_result = find_descendants_result__rx_handler;

    return SYS_ERR_OK;
}
