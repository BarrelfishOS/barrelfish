/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * scsi.c
 * Constains constructors to generate SCSI command blocks generators
 */

#include <stdio.h>
#include <usb/utility.h>
#include "scsi.h"


/*
 * \brief Prepares and packs INQUIRY command
 */

void get_inquiry_command(uint8_t len, inq_cmd_t * inq)
{
    inq->op_code = 0x12;
    // Use these macros because only 1 bit is set 
    inq->EVPD = EVPD_ZERO;
    inq->page_code = 0x0;
    inq->MSB = 0x0;
    inq->LSB = len;
    inq->ctrl = 0x0;
}

/*
 * \brief Prepares and packs READ_CAPCITY command
 */

void get_read_capacity_command(read_capacity_t * cmd)
{
    cmd->op_code = 0x25;
    cmd->rsvd = 0x0;
    cmd->lba = 0x0;
    cmd->rsvd1 = 0x0;
    cmd->pmi = 0x0;
    cmd->ctrl = 0x0;
}

/*
 * \brief Prepares and packs READ10 command. There are two version
 *        with and without cache. 
 */

void get_read10_command(read10_t * cmd, uint32_t block, uint16_t n,
                        uint8_t cache)
{
    cmd->cmd[0] = 0x28;
    if (cache == USE_CACHE)
        cmd->cmd[1] = (0x00) | (1 << 4 /* DPO */ ) |
            (0 << 3 /*FUA*/) | (0 << 1 /*FUA_NV */ );
    else
        cmd->cmd[1] = (0x00) | (1 << 4 /* DPO */ ) |
            (1 << 3 /*FUA*/) | (1 << 1 /*FUA_NV */ );

    cmd->cmd[2] = (block >> 24) & 0xFF;
    cmd->cmd[3] = (block >> 16) & 0xFF;
    cmd->cmd[4] = (block >> 8) & 0xFF;
    cmd->cmd[5] = block & 0xFF; // just last 8 bits 
    cmd->cmd[6] = 0;            // no grp 
    cmd->cmd[7] = (n >> 8) & 0xFF;
    cmd->cmd[8] = n & 0xFF;     // n logical blocks
    cmd->cmd[9] = 0;
}


/*
 * \brief Another flavor of read command. Not working !! 
 */

void get_read12_command(read12_t * cmd, uint32_t block, uint32_t n)
{
    cmd->cmd[0] = 0xA8;
    cmd->cmd[1] = (0x00) | (1 << 4 /* DPO */ ) | (1 << 3 /*FUA*/);
    // = cmd->cmd[2] = cmd->cmd[5] =0; 
    cmd->cmd[2] = (block >> 24) & 0xFF;
    cmd->cmd[3] = (block >> 16) & 0xFF;
    cmd->cmd[4] = (block >> 8) & 0xFF;
    cmd->cmd[5] = block & 0xFF; // just last 8 bits 

    cmd->cmd[6] = (n >> 24) & 0xFF;
    cmd->cmd[7] = (n >> 16) & 0xFF;
    cmd->cmd[8] = (n >> 8) & 0xFF;
    cmd->cmd[9] = n & 0xFF;     // just last 8 bits 

    cmd->cmd[10] = 0;           // No group 

    cmd->cmd[11] = 0;
}

/*
 * \brief Prepares and packs WRITE10 command. 
 */

void get_write10_command(write10_t * cmd, uint32_t block, uint16_t n,
                         uint8_t cache)
{
    printf("\n\n bloack addrwess %x\n\n", block);
    cmd->cmd[0] = 0x2A;
    if (cache == USE_CACHE)
        cmd->cmd[1] = (0x00) | (1 << 4 /* DPO */ ) |
            (0 << 3 /*FUA*/) | (0 << 1 /*FUA_NV */ );
    else
        cmd->cmd[1] = (0x00) | (1 << 4 /* DPO */ ) |
            (1 << 3 /*FUA*/) | (1 << 1 /*FUA_NV */ );


    cmd->cmd[2] = (block >> 24) & 0xFF;
    cmd->cmd[3] = (block >> 16) & 0xFF;
    cmd->cmd[4] = (block >> 8) & 0xFF;
    cmd->cmd[5] = block & 0xFF; // just last 8 bits 
    cmd->cmd[6] = 0;            // no grp 
    cmd->cmd[7] = (n >> 8) & 0xFF;
    cmd->cmd[8] = n & 0xFF;     // n logical blocks
    cmd->cmd[9] = 0;
}

/*
 * \brief SYNCS the volatile and persistent storage
 */

void get_sync_cache_command(sync_cache_t * cmd, uint32_t block, uint16_t n)
{
    printf("\n\n bloack addrwess %x\n\n", block);
    cmd->cmd[0] = 0x35;
    cmd->cmd[1] = (0x00);
    cmd->cmd[2] = (block >> 24) & 0xFF;
    cmd->cmd[3] = (block >> 16) & 0xFF;
    cmd->cmd[4] = (block >> 8) & 0xFF;
    cmd->cmd[5] = block & 0xFF; // just last 8 bits 
    cmd->cmd[6] = 0;            // no grp 
    cmd->cmd[7] = (n >> 8) & 0xFF;
    cmd->cmd[8] = n & 0xFF;     // n logical blocks
    cmd->cmd[9] = 0;
}


/*
 *
 * Various SCSI command dump printing routines
 */

void print_inq_data(inq_data_t iq)
{
    printf("\n\n-------- Dump of INQUIRY data dump (in HEX)");
    printf("\n [0] Dev type %x", iq.dev_type);
    printf("\n [1] RMB      %x", iq.RMB);
    printf("\n [2] Version %x", iq.version);
    printf("\n [3] Resp_data_format %x", iq.resp_data_format);
    printf("\n [4] Additonal_length %x", iq.additional_length);
    printf("\n [5] RSVD[0]  %x", iq.rsvd[0]);
    printf("\n [6] RSVD[1]  %x", iq.rsvd[1]);
    printf("\n [7] RSVD[2]  %x", iq.rsvd[2]);
    printf("\n [8-15] Vendor info : ");
    print_str((char *)&iq.vendor_info, 8);
    printf("\n [16-31] prod_id : ");
    print_str((char *)&iq.prod_id, 16);
    printf("\n [32-35] prod_revision : ");
    print_str((char *)&iq.prod_revision, 4);
}

/*
 * \brief Parse the capacity command request data and extract the 
 *        block size and last addressable block
 */

int init_scsi_dev(read_capacity_data_t d, scsi_device_t * dev)
{
    // XXX: assuming little endian machine 
    dev->block_last = (d.data[0] << 24)
        | (d.data[1] << 16) | (d.data[2] << 8) | (d.data[3]);

    dev->block_size = (d.data[4] << 24) |
        (d.data[5] << 16) | (d.data[6] << 8) | (d.data[7]);
    dev->valid = 1;
    return 0;
}
