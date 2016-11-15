/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "buffer_tbl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include "sfn5122f_debug.h"




static uint64_t buffer_offset = 0;
static uint64_t userspace_offset = 512;

// add a buffer to the buffer table
uint64_t alloc_buf_tbl_entries(uint64_t phys_addr,
                               uint32_t num_buf,
                               uint16_t qid,
                               bool userspace,
                               sfn5122f_t *d)
{
    uint64_t reg = 0;
 
    if (buffer_offset + num_buf > MAX_BUF_TBL_ENTRIES) {
        return -1;
    }

    for (int i = 0; i < num_buf; i++){
        reg = 0;
        reg = sfn5122f_buf_full_tbl_buf_adr_region_insert(reg, 0 );
        reg = sfn5122f_buf_full_tbl_buf_adr_fbuf_insert(reg,
                                    ((phys_addr+i*4096) >> 12));
        if (userspace) {
            reg = sfn5122f_buf_full_tbl_buf_owner_id_fbuf_insert(reg, qid+1);
            DEBUG_BUF("Buffer user address %lx o_id %d buf_id %ld \n", 
                      phys_addr+i*4096, qid+1, userspace_offset+i);
        } else {
            DEBUG_BUF("Buffer address %lx o_id %d buf_id %ld \n", 
                      phys_addr+i*4096, 0, buffer_offset+i);
            reg = sfn5122f_buf_full_tbl_buf_owner_id_fbuf_insert(reg, 0);
        }

        if (userspace) {
            sfn5122f_buf_full_tbl_wr(d, userspace_offset + i, reg);
        } else {
            sfn5122f_buf_full_tbl_wr(d, buffer_offset + i, reg);
        }
    }
  
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_upd_cmd_insert(reg, 1);
    sfn5122f_buf_tbl_upd_reg_lo_wr(d, reg); 
    sfn5122f_buf_tbl_upd_reg_hi_wr(d, 0); 

    if (userspace) {
        userspace_offset += num_buf;
    } else {
        buffer_offset += num_buf;
    }

    if (userspace) {
        return (userspace_offset - num_buf);
    }

    return (buffer_offset - (num_buf));
}


void free_buf_tbl_entries(uint64_t buftbl_idx,
                     uint32_t num_buf,
                     sfn5122f_t *d)
{
    uint64_t reg;
    reg = 0;
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_cmd_insert(reg, 1);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_start_id_insert(reg, buftbl_idx);
    reg = sfn5122f_buf_tbl_upd_reg_lo_buf_clr_end_id_insert(reg, buftbl_idx+num_buf);
    sfn5122f_buf_tbl_upd_reg_lo_wr(d, reg); 
    sfn5122f_buf_tbl_upd_reg_hi_wr(d, 0); 
}

