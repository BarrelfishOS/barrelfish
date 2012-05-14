#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>
#include <barrelfish/waitset.h>

#include "ARP_lookup_client.h"

#include <if/net_ARP_defs.h>
#include <if/net_ARP_rpcclient_defs.h>


// *****************************************************************
// Dealing with new connections
// *****************************************************************
static void net_ARP_bind_cb(void *st, errval_t err, struct net_ARP_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed for net_ARP");
        abort();
    }
    debug_printf("net_ARP_bind_cb: called\n");
    struct netg_interface* netif = st;

    err = net_ARP_rpc_client_init(&netif->net_ARP_rpc, b);
    if (!err_is_ok(err)) {
        printf("net_ARP_bind_cb failed in init\n");
        abort();
    }

    netif->net_ARP_service_connected = true;
    debug_printf("net_ARP_bind_cb: net_ARP bind successful!\n");
}

static void init_net_ARP_connection(struct netg_interface* netif, char *service_name)
{
	debug_printf("init_net_ARP_connection: called\n");
    assert(service_name != NULL);
    debug_printf("init_net_ARP_connection: connecting to [%s]\n", service_name);

    errval_t err;
    iref_t iref;

    debug_printf("init_net_ARP_connection: resolving driver %s\n", service_name);

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the net_ARP driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    debug_printf("init_net_ARP_connection: connecting\n");

    err = net_ARP_bind(iref, net_ARP_bind_cb, netif, get_default_waitset(),
                    IDC_BIND_FLAGS_DEFAULT);
    if (!err_is_ok(err)) {
        printf("net_ARP_bind_cb failed in init\n");
        abort();
    }

    debug_printf("init_net_ARP_connection: terminated\n");
}


// Connects to the port manager service
// Blocking call: returns only when connection is done
// In case of error, it will panic!!
void idc_connect_ARP_lookup_service(struct netg_interface* netif, char *service_name)
{
    //LWIPBF_DEBUG
    printf("idc_c_ARP_lookup_srv: trying to [%s]\n", service_name);

    /* FIXME: decide if this is the best place to connect with net_ARP */
    init_net_ARP_connection(netif, service_name);

    // XXX: dispatch on default waitset until bound
    struct waitset *dws = get_default_waitset();

    while (!netif->net_ARP_service_connected) {
        errval_t err = event_dispatch(dws);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch while binding ARP_service");
        }
    }
    //LWIPBF_DEBUG
    printf("idc_c_ARP_lookup_srv: success [%s]\n", service_name);
}


// ************************************************************************
//                 ARP lookup interface function
// ************************************************************************

errval_t idc_get_ip_from_ARP_lookup(struct netg_interface* netif, ip_addr_t *ip,
															ip_addr_t *gw,
															ip_addr_t *nm)
{
	debug_printf("On the way of getting IP via ARP lookup\n");

	errval_t remote_err;

	errval_t err = netif->net_ARP_rpc.vtbl.ip_info(&netif->net_ARP_rpc, netif->net_interface, &remote_err, ip, gw, nm);
	if (err_is_fail(remote_err)) {
		USER_PANIC_ERR(remote_err, "error in getting ip_info");
	}

	return err;
}

uint64_t idc_ARP_lookup(struct netg_interface* netif, uint32_t ip)
{
	debug_printf("idc_ARP_lookup: On the way of ARP lookup\n");

    errval_t err;
    errval_t remote_err;
    bool force = false;
    uint64_t mac = 0;

    err = netif->net_ARP_rpc.vtbl.ARP_lookup(&netif->net_ARP_rpc, ip, netif->net_interface, force,
            &remote_err, &mac);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in making ARP_lookup call");
    }

    if (err_is_fail(remote_err)) {
        USER_PANIC_ERR(remote_err, "error in ARP lookup process");
    }
    assert(mac != 0);

    debug_printf("idc_ARP_lookup: got answer\n");
    return mac;
} // end function: idc_ARP_lookup
