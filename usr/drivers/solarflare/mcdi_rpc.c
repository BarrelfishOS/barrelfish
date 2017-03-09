/*
 *Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include "sfn5122f.h"
#include "mcdi_rpc.h"
#include "sfn5122f_debug.h"

static uint32_t seqno = 0;

static struct thread_mutex mcdi;

void init_mcdi_mutex(void)
{
     thread_mutex_init(&mcdi);
}


/* Function to issue MCDI calls
 *****************************************************************
 * Each MCDI request starts with an MCDI_HEADER, which is a 32byte
 * structure, filled in by the client.
 *
 *       0       7  8     16    20     22  23  24    31
 *      | CODE | R | LEN | SEQ | Rsvd | E | R | XFLAGS |
 *               |                      |   |
 *               |                      |   \--- Response
 *               |                      \------- Error
 *               \------------------------------ Resync (always set)
*/

errval_t mcdi_rpc(unsigned cmd, const uint8_t *in, uint32_t inlen,
                     uint8_t *out, uint32_t outlen, uint32_t *outlen_actual, 
                     bool port, sfn5122f_t *d)
{
    uint32_t rlen = 0;
    uint32_t hdr = 0; 
    uint32_t reg = 0;
    uint32_t offset = MCDI_PDU(port);
    uint32_t offset2 = MCDI_DORBELL(port);
    unsigned error; 
    uint32_t* pointer = (uint32_t*) in;

    thread_mutex_lock(&mcdi);

    seqno++;

    // Code
    hdr |= cmd;
    // Resync
    hdr |= 1 << 7;
    // Len  
    hdr |= inlen << 8;
    // SEQ
    hdr |= seqno << 16;

    // write command 
    sfn5122f_mc_treg_smem_wr(d,offset,hdr);
    // write arguments
    for (int i = 0; i < inlen/4; i++) {   
        sfn5122f_mc_treg_smem_wr(d, offset+1+i, (uint32_t) pointer[i]);
        __sync_synchronize();
    }

    // from driver
    // ring dorbell with distinct value
    offset2 = MCDI_DORBELL(port);
    sfn5122f_mc_treg_smem_wr(d, offset2, 0x45789abc);
    // Poll for completion
    while(1){
        // TODO add backoff ? 
        reg = sfn5122f_mc_treg_smem_rd(d, offset);
        // If the reg is 0xffffffff the memory resets
        if (reg != 0xffffffff && ((reg >> 23) & 0x001))
            break;
    }

    error = (reg >> 22) & 0x001;
    rlen = (reg >> 8) & 0x0FF;

    if (cmd == CMD_REBOOT && error && !rlen) {
        DEBUG("CARD REBOOTED \n");
    } else if(error && !(cmd == CMD_REBOOT)){
        // TODO ERROR HANDLING
        reg = sfn5122f_mc_treg_smem_rd(d,offset+1);
        DEBUG("AN ERROR OCCURRED: CMD %d, ERROR %d \n",cmd , reg);
        switch(reg){
        case 4:
            return NIC_ERR_INTR;
        case 5:
            return NIC_ERR_IO;
        case 37:
            return NIC_ERR_NOSYS;
        default:
            return NIC_ERR_UNKNOWN;
        }
    }
    // read result
    // outlen computation from driver 
    outlen = (outlen < rlen+3) ? outlen : rlen+3;
    outlen = outlen & ~0x3;

    if (outlen_actual != NULL) {
        *outlen_actual = rlen;
    }

    memset(&out, outlen, 0);

    for (int i = 0; i < outlen; i+=4 ){
        reg = sfn5122f_mc_treg_smem_rd(d, offset+1+i/4);
        memcpy(out+i, &reg, 4);
    }

    __sync_synchronize();
    thread_mutex_unlock(&mcdi);
    return SYS_ERR_OK;
} 






