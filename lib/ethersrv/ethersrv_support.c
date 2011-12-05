/**
 * \file
 * \brief A support file holding benchmark and debug related code
 */

/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "ethersrv_support.h"

static void print_app_stats(struct buffer_descriptor *buffer)
{
    uint16_t i = 0;
    uint64_t avg = 0;
    uint64_t sd = 0;

    uint64_t n_sum = 0;
    uint64_t stat_sum = 0;
    uint64_t stat_min = 0;
    uint64_t stat_max = 0;

    struct pbuf_desc *pbuf = (struct pbuf_desc *) (buffer->pbuf_metadata_ds);


    uint8_t et = PBUF_REGISTERED;  // event type
    for (i = 0; i < RECEIVE_BUFFERS; ++i){
        avg = my_avg(pbuf[i].event_sum[et], pbuf[i].event_n[et]);
        sd = (pbuf[i].event_sum2[et] - (my_avg(pbuf[i].event_sum2[et], avg)) )/
            (pbuf[i].event_n[et] - 1);
/*
        printf("pbuf %"PRIu16": N[%"PRIu64"], AVG[%"PRIu64"], SD[%"PRIu64"],"
               "MAX[%"PRIu64"], MIN[%"PRIu64"]\n",
                i, pbuf[i].event_n[et], avg, sd,
                pbuf[i].event_max[et], pbuf[i].event_min[et]);
*/

        // Averaging above stats
        n_sum += pbuf[i].event_n[et];
        uint64_t to_avg = avg;
        stat_sum += to_avg;
        if (i == 0) {
            stat_min = to_avg;
            stat_max = to_avg;
        } else {
            if (to_avg > stat_max) {
                stat_max = to_avg;
            }
            if (to_avg < stat_min) {
                stat_min = to_avg;
            }
        }
    } // end for
/*
    printf("For %s (%"PRIu8"): N[%"PRIu64"], AVG[%"PRIu64"],"
            "MAX[%"PRIu64"], MIN[%"PRIu64"]\n",
            "PBUF_REGISTER", et, (n_sum / (RECEIVE_BUFFERS)),
            (stat_sum/RECEIVE_BUFFERS),
            (stat_max), (stat_min));
*/

    printf("For %s (%"PRIu8"): N[%"PRIu64"], AVG[%"PU"],"
            "MAX[%"PU"], MIN[%"PU"]\n",
            "PBUF_REGISTER", et, my_avg(n_sum, (RECEIVE_BUFFERS)),
          in_seconds(my_avg(stat_sum, (RECEIVE_BUFFERS))),
          in_seconds(stat_max), in_seconds(stat_min));
} // end function: reset_stats


static void bm_print_interesting_stats(uint8_t type)
{
    switch (type) {
        case 0:
            netbench_print_event_stat(bm, RE_PROCESSING_ALL,
                    "D: RX processing time", 1);
            netbench_print_event_stat(bm, RE_USEFUL, "D: RX useful time", 1);
            netbench_print_event_stat(bm, RE_FILTER,  "D: RX Filter time", 1);
            netbench_print_event_stat(bm, RE_COPY, "D: RX copy time", 1);
            netbench_print_event_stat(bm, RE_PBUF_REG, "D: RX pbuf reg T", 1);
            netbench_print_event_stat(bm, RE_DROPPED, "D: RX dropped time", 1);
            netbench_print_event_stat(bm, RE_TX_SP_MSG, "D: SP_MSG", 1);
            netbench_print_event_stat(bm, RE_TX_SP_MSG_Q, "D: SP_MSG_Q", 1);
            netbench_print_event_stat(bm, RE_PENDING_WORK, "D: pending wrk", 1);
            break;

        case 1:
            netbench_print_event_stat(bm, RE_TX_NOTI_CS,
                    "D: NOTI FROM APPLI", 0);
            netbench_print_event_stat(bm, RE_TX_T, "D: TX T", 0);
            netbench_print_event_stat(bm, RE_TX_SP_S, "D: TX SP_S", 0);
            netbench_print_event_stat(bm, RE_TX_SP_F, "D: TX SP_F", 0);
            netbench_print_event_stat(bm, RE_TX_DONE, "D: TX DONE", 0);
            netbench_print_event_stat(bm, RE_TX_W_ALL, "D: TX W_ALL", 0);
            netbench_print_event_stat(bm, RE_TX_DONE_NN, "D: TX DONE_NN", 0);
            netbench_print_event_stat(bm, RE_TX_DONE_N, "D: TX DONE_N", 0);
            netbench_print_event_stat(bm, RE_TX_SP_MSG, "D: TX SP_MSG", 0);
            netbench_print_event_stat(bm, RE_TX_SP_MSG_Q, "D: TX SP_MSG_Q", 0);
        break;

        default:
            printf("Invalid type given to print stats\n");
    } // end switch
}



static void reset_app_stats(struct buffer_descriptor *buffer)
{
    int i = 0;
    struct pbuf_desc *pbuf = (struct pbuf_desc *) (buffer->pbuf_metadata_ds);

    for (i = 0; i < RECEIVE_BUFFERS; ++i){
        for (int j = 0; j < MAX_STAT_EVENTS; ++j) {
            pbuf[i].event_ts[j] = 0;
            pbuf[i].event_n[j] = 0;
            pbuf[i].event_sum[j] = 0;
            pbuf[i].event_sum2[j] = 0;
            pbuf[i].event_max[j] = 0;
            pbuf[i].event_min[j] = 0;
            pbuf[i].event_sum_i[j] = 0;
            pbuf[i].event_sum2_i[j] = 0;
            pbuf[i].event_max_i[j] = 0;
            pbuf[i].event_min_i[j] = 0;
        }
    } // end for
} // end function: reset_stats


void add_event_stat(struct pbuf_desc *pbuf_d, int event_type)
{
    uint64_t ts = rdtsc();
    uint64_t delta = 0;
//    uint64_t delta_i = 0;
    if(pbuf_d->event_n[event_type] > 0) {
        delta = ts - pbuf_d->event_ts[event_type];
    }
    pbuf_d->event_sum[event_type] += delta;
    pbuf_d->event_sum2[event_type] += ( delta * delta);

    // Recording max, min
    if(pbuf_d->event_n[event_type] == 1) {
        pbuf_d->event_max[event_type] = delta;
        pbuf_d->event_min[event_type] = delta;
    } else {
        if (delta > pbuf_d->event_max[event_type]) {
            pbuf_d->event_max[event_type] = delta;
        }
        if (delta < pbuf_d->event_min[event_type]) {
            pbuf_d->event_max[event_type] = delta;
        }
    }
    ++pbuf_d->event_n[event_type];
    pbuf_d->event_ts[event_type] = ts;
    // FIXME: collect stats for incremental events as well
}


void reset_client_closure_stat(struct client_closure *cc)
{
    cc->start_ts = 0;
    cc->start_ts_tx = 0;
    cc->pkt_count = 0;
    cc->hw_queue = 0;
    cc->tx_explicit_msg_needed = 0;
    cc->tx_notification_sent = 0;
    cc->dropped_pkt_count = 0;
    cc->in_dropped_q_full = 0;
    cc->in_dropped_invalid_pkt = 0;
    cc->in_dropped_no_app = 0;
    cc->in_dropped_app_buf_full = 0;
    cc->in_dropped_app_invalid_buf = 0;
    cc->in_dropped_notification_prob = 0;
    cc->in_dropped_notification_prob2 = 0;
    cc->tx_done_count = 0;
    cc->in_dropped_q_full = 0;
    cc->in_success = 0;
    cc->in_trigger_counter = 0;
    cc->out_trigger_counter = 0;
    cc->filter_matched = 0;
    cc->in_other_pkts = 0;
    cc->in_arp_pkts = 0;
    cc->in_netd_pkts = 0;
    cc->in_paused_pkts = 0;
    cc->in_filter_matched = 0;
    cc->in_filter_matched_p = 0;
    cc->in_filter_matched_f = 0;
    cc->in_queue_len_n = 0;
    cc->in_queue_len_sum = 0;
    cc->in_app_time_n = 0;
    cc->in_app_time_sum = 0;
    cc->in_app_time_min = 0;
    cc->in_app_time_max = 0;
    cc->pbuf_count = 0;
}


static errval_t send_benchmark_control_response(struct q_entry entry)
{
    //    ETHERSRV_DEBUG("send_mac_addr_response -----\n");
    struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.benchmark_control_response(b,
                           MKCONT(cont_queue_callback, ccl->q),
                           entry.plist[0], entry.plist[1], entry.plist[2]);
                // state, delta, cl
    } else {
        ETHERSRV_DEBUG("send_benchmark_control_response Flounder busy.."
                " will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void send_benchmark_control(struct ether_binding *cc, uint64_t state,
        uint64_t delta, uint64_t cl)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_benchmark_control_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = state;
    entry.plist[1] = delta;
    entry.plist[2] = cl;
    /* entry.plist[0]);
       entry.hwaddr */
    enqueue_cont_q(ccl->q, &entry);
}


void benchmark_control_request(struct ether_binding *cc, uint8_t state,
        uint64_t trigger, uint64_t cl_data)
{
    uint64_t ts;
    uint8_t bm_type = 0; // 0 = RX benchmark, 1 = TX benchmark
//    printf("setting the debug status to %x and trigger [%"PRIu64"]\n",
//            state, trigger);
    struct client_closure *cl = ((struct client_closure *) (cc->st));

    cl->debug_state = state;
    switch (state) {

        case BMS_STOP_REQUEST:  // PRINTING stats

            if (bm_type == 0) {
                ts = rdtsc() - cl->start_ts;
            } else {
                ts = rdtsc() - cl->start_ts_tx;
            }
            printf("# D: Stopping MBM time[%"PU"],"
                   "TX Pbufs[%" PRIu64 "], TX pkts[%" PRIu64 "], "
                   "D[%" PRIu64 "], "
                   "in SP Q[%" PRIu64 "], HW_Q[%"PRIu64"]\n",
                   in_seconds(ts), cl->pbuf_count, cl->pkt_count,
                   cl->dropped_pkt_count,
                   sp_queue_elements_count(cl->spp_ptr),
                   cl->hw_queue);

            printf( "# D: RX FM[%"PRIu64"], FMF[%"PRIu64"], FMP[%"PRIu64"]\n",
                  cl->in_filter_matched, cl->in_filter_matched_f,
                  cl->in_filter_matched_p);

            uint64_t data_size = cl->in_filter_matched_p * 1330;
            printf("# D: RX speed (D view) = data(%"PRIu64") / time(%"PU") "
                    "= [%f] KB \n",
                    data_size,
                    in_seconds(ts), ((data_size/in_seconds(ts))/1024));

/*
            printf("### RX OK[%"PRIu64"], D_CQ_full[%"PRIu64"], "
                  "D_invalid[%"PRIu64"], D_NO_APP[%"PRIu64"], "
                  "D_APP_BUF_FULL[%"PRIu64"], D_APP_BUF_INV[%"PRIu64"]\n",
                  cl->in_success, cl->in_dropped_q_full,
                  cl->in_dropped_invalid_pkt, cl->in_dropped_no_app,
                  cl->in_dropped_app_buf_full, cl->in_dropped_app_invalid_buf);
            printf("### RX D_NTF_PROB[%"PRIu64"], D_NTF_PRO2[%"PRIu64"], "
                  "Other_pkt_OK[%"PRIu64"], in_ARP[%"PRIu64"], "
                  "in_NETD[%"PRIu64"], in_paused[%"PRIu64"]\n",
                  cl->in_dropped_notification_prob,
                  cl->in_dropped_notification_prob2, cl->in_other_pkts,
                  cl->in_arp_pkts, cl->in_netd_pkts, cl->in_paused_pkts);
*/
            printf( "# D: RX APP N[%"PRIu64"] avg[%"PU"], MAX[%"PU"]"
                    "MAX[%"PU"]\n", cl->in_app_time_n,
                    in_seconds(my_avg(cl->in_app_time_sum,cl->in_app_time_n)),
                    in_seconds(cl->in_app_time_max),
                    in_seconds(cl->in_app_time_min));

            print_app_stats(cl->buffer_ptr);
            bm_print_interesting_stats(bm_type);

            printf("D: Interrupt count [%"PRIu64"], loop count[%"PRIu64"], "
                    "Time in interrupt handling [%"PU"]\n",
                    interrupt_counter, total_rx_p_count,
                    in_seconds(total_interrupt_time));

            printf("D: Driver spp state");
            sp_print_metadata(cl->spp_ptr);

            send_benchmark_control(cc, BMS_STOPPED, ts,
                    (cl->pkt_count - cl->dropped_pkt_count));

            cl->in_trigger_counter = trigger;
            cl->out_trigger_counter = trigger;

            cl->debug_state = BMS_STOPPED;
            cl->debug_state_tx = BMS_STOPPED;
            g_cl = NULL;
            break;

        case BMS_START_REQUEST:  // Resetting stats, for new round of recording
            interrupt_counter = 0;
            total_rx_p_count = 0;
            total_interrupt_time = 0;
            g_cl = cl;
            reset_client_closure_stat(cl);
            // FIXME: Remove it, only for specific debugging!!!!
            if (cl->spp_ptr->sp->read_reg.value != 0) {
                printf("# D: reset_client_closure_stat: read_reg == %"PRIu64""
                    "instead of 0\n",
                cl->spp_ptr->sp->read_reg.value);
            }
//            assert(cl->spp_ptr->sp->read_reg.value == 0);

            cl->in_trigger_counter = trigger;
            cl->out_trigger_counter = trigger;
            cl->debug_state = 3;
            cl->debug_state_tx = 3;
            cl->pkt_count = 0;
            // Resetting receive path stats
            reset_app_stats(cl->buffer_ptr);
            netbench_reset(bm);
            bm->status = 1;
            printf("# D: Starting MBM now \n");
            cl->start_ts = rdtsc();
            cl->start_ts_tx = rdtsc();
            send_benchmark_control(cc, BMS_RUNNING, cl->start_ts, trigger);
            break;


        default:
            printf("# D: MBM: invalid state %x \n", state);
    } // end switch:

} // end function: benchmark_control_request

