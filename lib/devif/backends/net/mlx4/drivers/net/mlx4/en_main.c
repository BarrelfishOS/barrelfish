/*
 * Copyright (c) 2007, 2014 Mellanox Technologies. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenIB.org BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */

/*#include <linux/module.h>
 #include <linux/delay.h>
 #include <linux/netdevice.h>
 #include <linux/slab.h>

 #include <linux/mlx4/driver.h>
 */
#include <linux/mlx4/device.h>

#include <linux/mm.h>
#include <linux/log2.h>

#include <mlx4en.h>

#include <mlx4en.h>

#include <debug.h>
#include <unistd.h>

#include "mlx4_en.h"

/*
 #include <linux/mlx4/cmd.h>

 #include "mlx4_en.h"

 Mellanox ConnectX HCA Ethernet driver
 */

#define MLX4_EN_PARM_INT(X, def_val, desc) \
	static unsigned int X = def_val;/*\
	module_param(X , uint, 0444); \
	MODULE_PARM_DESC(X, desc);*/

/*
 * Device scope module parameters
 */

/*Enable RSS UDP traffic*/
MLX4_EN_PARM_INT(udp_rss, 1, "Enable RSS for incoming UDP traffic");
/*Priority pausing*/
MLX4_EN_PARM_INT(pfctx, 0,
		"Priority based Flow Control policy on TX[7:0]." " Per priority bit mask");
MLX4_EN_PARM_INT(pfcrx, 0,
		"Priority based Flow Control policy on RX[7:0]." " Per priority bit mask");

#define MAX_PFC_TX	0xff
#define MAX_PFC_RX	0xff

static int mlx4_en_get_profile(struct mlx4_en_dev *mdev) {
	struct mlx4_en_profile *params = &mdev->profile;
	int i, mp_ncpus;

	params->udp_rss = udp_rss;
	mp_ncpus = 4; //sysconf(_SC_NPROCESSORS_ONLN);
	/*HARDWIRE: use the number of active CPUs instead of 4*/
	params->num_tx_rings_p_up = min_t(int, mp_ncpus,
			MLX4_EN_MAX_TX_RING_P_UP);
	if (params->udp_rss
			&& !(mdev->dev->caps.flags & MLX4_DEV_CAP_FLAG_UDP_RSS)) {
		MLX4_WARN("UDP RSS is not supported on this device.\n");
		params->udp_rss = 0;
	}
	for (i = 1; i <= MLX4_MAX_PORTS; i++) {
		params->prof[i].rx_pause = 1;
		params->prof[i].rx_ppp = pfcrx;
		params->prof[i].tx_pause = 1;
		params->prof[i].tx_ppp = pfctx;
		params->prof[i].tx_ring_size = MLX4_EN_DEF_TX_RING_SIZE;
		params->prof[i].rx_ring_size = MLX4_EN_DEF_RX_RING_SIZE;
		params->prof[i].tx_ring_num =
				params->num_tx_rings_p_up * MLX4_EN_NUM_UP;
		params->prof[i].rss_rings = 0;
	}

	return 0;
}
/*
 static void *mlx4_en_get_netdev(struct mlx4_dev *dev, void *ctx, u8 port)
 {
 struct mlx4_en_dev *endev = ctx;

 return endev->pndev[port];
 }

 static void mlx4_en_event(struct mlx4_dev *dev, void *endev_ptr,
 enum mlx4_dev_event event, unsigned long port)
 {
 struct mlx4_en_dev *mdev = (struct mlx4_en_dev *) endev_ptr;
 struct mlx4_en_priv *priv;

 switch (event) {
 case MLX4_DEV_EVENT_PORT_UP:
 case MLX4_DEV_EVENT_PORT_DOWN:
 if (!mdev->pndev[port])
 return;
 priv = netdev_priv(mdev->pndev[port]);
 To prevent races, we poll the link state in a separate
 task rather than changing it here
 priv->link_state = event;
 queue_work(mdev->workqueue, &priv->linkstate_task);
 break;1,

 case MLX4_DEV_EVENT_CATASTROPHIC_ERROR:
 MLX4_ERR( "Internal error detected, restarting device\n");
 break;

 case MLX4_DEV_EVENT_SLAVE_INIT:
 case MLX4_DEV_EVENT_SLAVE_SHUTDOWN:
 break;
 default:
 if (port < 1 || port > dev->caps.num_ports ||
 !mdev->pndev[port])
 return;
 MLX4_WARN( "Unhandled event %d for port %d\n", event,
 (int) port);
 }
 }

 static void mlx4_en_remove(struct mlx4_dev *dev, void *endev_ptr)
 {
 struct mlx4_en_dev *mdev = endev_ptr;
 int i, ret;

 mutex_lock(&mdev->state_lock);
 mdev->device_up = false;
 mutex_unlock(&mdev->state_lock);

 mlx4_foreach_port(i, dev, MLX4_PORT_TYPE_ETH)
 if (mdev->pndev[i])
 mlx4_en_destroy_netdev(mdev->pndev[i]);

 flush_workqueue(mdev->workqueue);
 destroy_workqueue(mdev->workqueue);
 ret = mlx4_mr_free(dev, &mdev->mr);
 if (ret)
 MLX4_ERR( "Error deregistering MR. The system may have become unstable.");
 iounmap(mdev->uar_map);
 mlx4_uar_free(dev, &mdev->priv_uar);
 mlx4_pd_free(dev, mdev->priv_pdn);
 kfree(mdev);
 }*/

void *mlx4_en_add(struct mlx4_dev *dev) {
	struct mlx4_en_dev *mdev;
	int i;
	int err;

	mdev = calloc(1, sizeof *mdev);
	if (!mdev) {
		MLX4_ERR("Device struct alloc failed, "
				"aborting.\n");
		err = -ENOMEM;
		goto err_free_res;
	}

	if (mlx4_pd_alloc(dev, &mdev->priv_pdn))
		goto err_free_dev;

	if (mlx4_uar_alloc(dev, &mdev->priv_uar))
		goto err_pd;

	mdev->uar_map = (void *) (mdev->priv_uar.pfn << PAGE_SHIFT);
	if (!mdev->uar_map)
		goto err_uar;
	/*spin_lock_init(&mdev->uar_lock);*/

	mdev->dev = dev;
	/*mdev->dma_device = &(dev->pdev->dev);
	 mdev->pdev = dev->pdev;*/
	mdev->device_up = false;

	mdev->LSO_support = !!(dev->caps.flags & (1 << 15));
	if (!mdev->LSO_support)
		MLX4_WARN(
				"LSO not supported, please upgrade to later " "FW version to enable LSO\n");

	if (mlx4_mr_alloc(mdev->dev, mdev->priv_pdn, 0, ~0ull,
			MLX4_PERM_LOCAL_WRITE | MLX4_PERM_LOCAL_READ, 0, 0, &mdev->mr)) {
		MLX4_ERR("Failed allocating memory region\n");
		goto err_map;
	}
	if (mlx4_mr_enable(mdev->dev, &mdev->mr)) {
		MLX4_ERR("Failed enabling memory region\n");
		goto err_mr;
	}

	/* Build device profile according to supplied module parameters */
	err = mlx4_en_get_profile(mdev);
	if (err) {
		MLX4_ERR("Bad module parameters, aborting.\n");
		goto err_mr;
	}

	/* Configure which ports to start according to module parameters */
	mdev->port_cnt = 0;
	mlx4_foreach_port(i, dev, MLX4_PORT_TYPE_ETH)
		mdev->port_cnt++;

	mlx4_foreach_port(i, dev, MLX4_PORT_TYPE_ETH)
	{
		if (!dev->caps.comp_pool) {
		mdev->profile.prof[i].rx_ring_num =
		rounddown_pow_of_two(max_t(int, MIN_RX_RINGS,
						min_t(int,
								dev->caps.num_comp_vectors,
								DEF_RX_RINGS)));
    	} else {
    	mdev->profile.prof[i].rx_ring_num = rounddown_pow_of_two(
    			min_t(int, dev->caps.comp_pool /
    					dev->caps.num_ports, MAX_MSIX_P_PORT));
        }
    }

    /* Create our own workqueue for reset/multicast tasks
     * Note: we cannot use the shared workqueue because of deadlocks caused
     *       by the rtnl lock */
    /*mdev->workqueue = create_singlethread_workqueue("mlx4_en");
     if (!mdev->workqueue) {
     err = -ENOMEM;
     goto err_mr;
     }*/

    /* At this stage all non-port specific tasks are complete:
     * mark the card state as up */
    /*mutex_init(&mdev->state_lock);*/
    mdev->device_up = true;

    /* Setup ports */

    /* Create a netdev for each port */
    /*mlx4_foreach_port(i, dev, MLX4_PORT_TYPE_ETH)
     {*/
    i = 1;
    MLX4_DEBUG("Activating port:%d\n", i);
    if (mlx4_en_init_netdev(mdev, i, &mdev->profile.prof[i]))
    mdev->pndev[i] = NULL;
    /*}*/

    return mdev;

    /*TODO: cleanup*/
    err_mr: /*err = mlx4_mr_free(dev, &mdev->mr);*/
    /*if (err)*/
    MLX4_ERR("Error deregistering MR. The system may have become unstable.");
    err_map: /*if (mdev->uar_map)
     iounmap(mdev->uar_map);*/
    err_uar: /*mlx4_uar_free(dev, &mdev->priv_uar);*/
    err_pd: /*mlx4_pd_free(dev, mdev->priv_pdn);*/
    err_free_dev: free(mdev);
    err_free_res: return NULL;
}

/*
 static struct mlx4_interface mlx4_en_interface = {
 .add		= mlx4_en_add,
 .remove		= mlx4_en_remove,
 .event		= mlx4_en_event,
 .get_dev	= mlx4_en_get_netdev,
 .protocol	= MLX4_PROT_ETH,
 };

 static void mlx4_en_verify_params(void)
 {
 if (pfctx > MAX_PFC_TX) {
 pr_warn("mlx4_en: WARNING: illegal module parameter pfctx 0x%x - "
 "should be in range 0-0x%x, will be changed to default (0)\n",
 pfctx, MAX_PFC_TX);
 pfctx = 0;
 }

 if (pfcrx > MAX_PFC_RX) {
 pr_warn("mlx4_en: WARNING: illegal module parameter pfcrx 0x%x - "
 "should be in range 0-0x%x, will be changed to default (0)\n",
 pfcrx, MAX_PFC_RX);
 pfcrx = 0;
 }
 }


 static int __init mlx4_en_init(void)
 {
 mlx4_en_verify_params();

 #ifdef CONFIG_DEBUG_FS
 int err = 0;
 err = mlx4_en_register_debugfs();
 if (err)
 pr_err(KERN_ERR "Failed to register debugfs\n");
 #endif
 return mlx4_register_interface(&mlx4_en_interface);
 }

 static void __exit mlx4_en_cleanup(void)
 {
 mlx4_unregister_interface(&mlx4_en_interface);
 #ifdef CONFIG_DEBUG_FS
 mlx4_en_unregister_debugfs();
 #endif
 }

 module_init(mlx4_en_init);
 module_exit(mlx4_en_cleanup);

 #undef MODULE_VERSION
 #include <sys/module.h>
 static int
 mlxen_evhand(module_t mod, int event, void *arg)
 {
 return (0);
 }
 static moduledata_t mlxen_mod = {
 .name = "mlxen",
 .evhand = mlxen_evhand,
 };
 DECLARE_MODULE(mlxen, mlxen_mod, SI_SUB_OFED_PREINIT, SI_ORDER_ANY);
 MODULE_DEPEND(mlxen, mlx4, 1, 1, 1);
 */
