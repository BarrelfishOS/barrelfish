/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Nothing special ...basically contains 
 * all the printing and dumping functions 
 * for EHCI interface data structs
 */

#include <stdio.h>

#include "ehci.h"

static void print_next_ptr(char *from, union h_ptr ptr, int i)
{
    printf("\n %s [%d]: [Link PTR %x] [Annon %x] [Typ %x] [T %x]", from, i,
           ptr.str.ptr, ptr.str.ann, ptr.str.typ, ptr.str.t);
    //printf("\n %s [%d]: Typ %x", from, i, ptr->next_qhead.str.typ);
    //printf("\n %s [%d]: Annon %x", from, i, ptr->next_qhead.str.annon);
    //printf("\n %s [%d]: Link PTR %x", from, i, ptr->next_qhead.str.ptr);
}

static void print_q_token(char *from, union qTD_token tk, int i)
{
    printf("\n %s [%d]: [dt %x]", from, i, tk.str.dt);
    printf(" [total_bytes %x]", tk.str.total_bytes);
    printf(" [ioc %x]", tk.str.ioc);
    printf(" [c_page %x]", tk.str.c_page);
    printf(" [cerr %x]", tk.str.cerr);
    printf(" [pid %x]", tk.str.pid);
    printf("[status %x]", tk.str.status);

    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
    //printf("\n %s [%d]: %x", from, i, ptr->q_token.str.);
}

static void print_buff_ptr(char *from, union qTD_buff_list buff, int i)
{
    printf("\n %s [%d]: [ptr %x] [offset %x]", from, i, buff.str.buff_ptr,
           buff.str.offset);
}

static void print_buff_ptr_arr(char *from, union qTD_buff_list *buff, int sz,
                               int start)
{
    int i;
    for (i = 0; i < sz; i++)
        print_buff_ptr(from, buff[i], start + i);

}

void print_qhead(struct q_head *qh)
{
    if (qh == NULL)
        return;

    printf("\n\n ---- EHCI QUEUE HEAD DUMP  VM [%p] ------ ", qh);
    print_next_ptr("QH", qh->next_qhead, 0);
    printf
        ("\n QH [1] [RL %x] [C %x] [packetlength %x] [H %x] [dtc %x] [eps %x] [endpt %x] [I %x] [device_address %x]",
         qh->ep_char.str.nack_reload, qh->ep_char.str.ctrl_endpt_flag,
         qh->ep_char.str.max_packet_length, qh->ep_char.str.head,
         qh->ep_char.str.dtc, qh->ep_char.str.eps, qh->ep_char.str.endpt_num,
         qh->ep_char.str.i, qh->ep_char.str.dev_addr);

    printf("\n QH [2] [mult %x] [port %x]  [Hub %x] [uMask-C %x] [uMask-S %x]",
           qh->ep_cap.str.mult,
           qh->ep_cap.str.port_num,
           qh->ep_cap.str.hub_addr,
           qh->ep_cap.str.c_mask, qh->ep_cap.str.s_mask);


    print_next_ptr("QH", qh->current, 3);
    print_next_ptr("QH", qh->next, 4);
    print_next_ptr("QH", qh->alt, 5);

    print_q_token("QH", qh->q_token, 6);
    print_buff_ptr_arr("QH", qh->buff_list, 5, 7);
    printf("\n ------- END OF QUEUE HEAD DUMP ----------- \n\n");

}

void print_qTD(struct qTD *ptr)
{
    if (ptr == NULL)
        return;

    printf("\n\n -------- EHCI qTD ELEM DUMP  VM [%p] --------------- ", ptr);

    print_next_ptr("qTD", ptr->next_qTD, 1);
    print_next_ptr("qTD", ptr->alt, 2);
    print_q_token("qTD", ptr->q_token, 3);
    print_buff_ptr_arr("qTD", ptr->buff_list, 5, 4);
    printf("\n -------- END OF EHCI qTD ELEM DUMP ------------\n\n ");
}
