/**
 * Implement the int_controller_client interface for MSIx
 */

#ifndef _MSIX_CLIENT_H_H_
#define _MSIX_CLIENT_H_H_

#include <barrelfish/barrelfish.h>
#include <int_route/int_model.h>


/**
 * Initialize the controller. Needs MSIx table base 
 * and the controller name. Alternatively, it tries
 * to parse out a int_model argument.
 */
errval_t msix_client_init_by_args(int argc, char **argv, void* msix_tab);
errval_t msix_client_init(struct int_startup_argument *arg, void* msix_tab);

#endif
