#include <linux/gfp.h>

#include <barrelfish/sys_debug.h>

#include <mlx4ib.h>

#define BUFF_SIZE 100
#define MAX_POLL 2000
void test_ib(struct ib_device *device) {
	struct ib_pd *pd;
	struct ib_cq *cq;
	struct ib_mr *mr;
	struct ib_qp *qp;
	void *buff_vaddr;
	u64 buff_paddr = 0;
	int mr_flags;
	struct ib_qp_init_attr attr;
	int ret;
	int flags;
	int uninitialized_var( poll_result);
	struct ib_qp_attr qp_attr;
	struct ib_send_wr sr;
	struct ib_sge sge;
	struct ib_send_wr *bad_wr = NULL;
	struct ib_wc wc;
	uint64_t current, ticks_per_msec;
	int opcode = IB_WR_RDMA_READ;

	pd = ib_alloc_pd(device);
	if (!pd)
		printf("failed to alloc PD\n");

	cq = ib_create_cq(device, NULL, NULL, NULL, 1, 0);
	if (!cq)
		printf("failed to create CQ\n");

	buff_vaddr = dma_alloc(BUFF_SIZE, &buff_paddr);
	if (!buff_vaddr)
		printf("failed to alloc buff\n");

	mr_flags = IB_ACCESS_LOCAL_WRITE | IB_ACCESS_REMOTE_READ
			| IB_ACCESS_REMOTE_WRITE;

	mr = ib_get_dma_mr(pd, mr_flags);
	printf(
			"MR was registered with addr=0x%lx, lkey=0x%x, rkey=0x%x, flags=0x%x\n",
			buff_paddr, mr->lkey, mr->rkey, mr_flags);
	/*mr = mlx4_ib_reg_user_mr(pd, (u64) buff, BUFF_SIZE, (u64) buff, mr_flags, NULL,
	 0);*/

	memset(&attr, 0, sizeof attr);
	attr.create_flags = 0;
	attr.event_handler = NULL;
	attr.qp_context = NULL;
	attr.send_cq = cq;
	attr.recv_cq = cq;
	attr.srq = NULL;
	attr.sq_sig_type = IB_SIGNAL_ALL_WR;
	attr.qp_type = IB_QPT_RC;
	attr.xrcd = NULL;
	attr.create_flags = 0;

	attr.cap.max_send_wr = 1;
	attr.cap.max_recv_wr = 1;
	attr.cap.max_send_sge = 1;
	attr.cap.max_recv_sge = 1;
	attr.cap.max_inline_data = 0;
	qp = ib_create_qp(pd, &attr);
	printf("QP was created, QP number=0x%x\n", qp->qp_num);

	memset(&qp_attr, 0, sizeof qp_attr);
	qp_attr.qp_state = IB_QPS_INIT;
	qp_attr.port_num = 2;
	qp_attr.pkey_index = 0;
	qp_attr.qp_access_flags = IB_ACCESS_LOCAL_WRITE | IB_ACCESS_REMOTE_READ
			| IB_ACCESS_REMOTE_WRITE;
	flags = IB_QP_STATE | IB_QP_PKEY_INDEX | IB_QP_PORT | IB_QP_ACCESS_FLAGS;
	ret = ib_modify_qp(qp, &qp_attr, flags);
	if (ret)
		printf("failed to modify QP state to INIT\n");

	memset(&qp_attr, 0, sizeof(qp_attr));
	qp_attr.qp_state = IB_QPS_RTR;
	qp_attr.path_mtu = IB_MTU_256;
	qp_attr.dest_qp_num = dest_qp_num; //XXX: need to be HARDWIRED
	qp_attr.rq_psn = 0;
	qp_attr.max_dest_rd_atomic = 1;
	qp_attr.min_rnr_timer = 0x12;
	qp_attr.ah_attr.ah_flags = 0;
	qp_attr.ah_attr.dlid = dest_lid; //XXX: need to be HARDWIRED
	qp_attr.ah_attr.sl = 0;
	qp_attr.ah_attr.src_path_bits = 0;
	qp_attr.ah_attr.port_num = 2;
	flags = IB_QP_STATE | IB_QP_AV | IB_QP_PATH_MTU | IB_QP_DEST_QPN
			| IB_QP_RQ_PSN | IB_QP_MAX_DEST_RD_ATOMIC | IB_QP_MIN_RNR_TIMER;
	ret = ib_modify_qp(qp, &qp_attr, flags);
	if (ret)
		printf("failed to modify QP state to RTS\n");

	memset(&qp_attr, 0, sizeof(qp_attr));
	qp_attr.qp_state = IB_QPS_RTS;
	qp_attr.timeout = 0x12;
	qp_attr.retry_cnt = 6;
	qp_attr.rnr_retry = 0;
	qp_attr.sq_psn = 0;
	qp_attr.max_rd_atomic = 1;
	flags = IB_QP_STATE | IB_QP_TIMEOUT | IB_QP_RETRY_CNT | IB_QP_RNR_RETRY
			| IB_QP_SQ_PSN | IB_QP_MAX_QP_RD_ATOMIC;
	ret = ib_modify_qp(qp, &qp_attr, flags);
	if (ret)
		printf("failed to modify QP state to RTS\n");

	/* prepare the scatter/gather entry */
	memset(&sge, 0, sizeof(sge));
	sge.addr = buff_paddr;
	sge.length = BUFF_SIZE;
	sge.lkey = mr->lkey;
	/* prepare the send work request */
	memset(&sr, 0, sizeof(sr));
	sr.next = NULL;
	sr.wr_id = 0;
	sr.sg_list = &sge;
	sr.num_sge = 1;
	sr.opcode = opcode;
	sr.send_flags = IB_SEND_SIGNALED;
	if (opcode != IB_WR_SEND) {
		/*XXX: the r_address and r_key has to be hardwired*/
		sr.wr.rdma.remote_addr = dest_addr;
		sr.wr.rdma.rkey = dest_rkey;
	}
	/* there is a Receive Request in the responder side, so we won't get any into RNR flow */
	ret = ib_post_send(qp, &sr, &bad_wr);
	if (ret) {
		printf("failed to post SR\n");
	} else {
		switch (opcode) {
		case IB_WR_SEND:
			printf("Send Request was posted\n");
			break;
		case IB_WR_RDMA_READ:
			printf("RDMA Read Request was posted\n");
			break;
		case IB_WR_RDMA_WRITE:
			printf("RDMA Write Request was posted\n");
			break;
		default:
			printf("Unknown Request was posted\n");
			break;
		}
	}

	current = rdtsc();
	sys_debug_get_tsc_per_ms(&ticks_per_msec);
	while (poll_result == 0 && rdtsc() < current + MAX_POLL * ticks_per_msec) {
		poll_result = ib_poll_cq(cq, 1, &wc);
	}
	if (poll_result < 0) {
		/* poll CQ failed */
		printf("poll CQ failed\n");
	} else if (poll_result == 0) {
		/* the CQ is empty */
		printf("completion wasn't found in the CQ after timeout\n");
	} else {
		/* CQE found */
		printf("completion was found in CQ with status 0x%x\n", wc.status);
		/* check the completion status (here we don't care about the completion opcode */
		if (wc.status != IB_WC_SUCCESS) {
			printf(
					"got bad completion with status: 0x%x, vendor syndrome: 0x%x\n",
					wc.status, wc.vendor_err);
		}
	}

	printf("Contents of client's buffer: %s\n", (char *) buff_vaddr);
}
