/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#ifndef SCSI_H
#define SCSI_H

/*
 * This file contains definitions and some helper utilities to generate 
 * SCSI Blocks commands (SCSI SBC-2).
 * 
 * Commands are taken from SCSI SPC-3 documentation at page 142-143-144 
 * for INQUIRY command rest are from SBC-2
 */

#define EVPD_ZERO 0
#define EVPD_ONE 1


#define USE_CACHE 1
#define DONT_USE_CACHE 0

/* 
 * Use byte alignment, otherwise sometimes gcc padds the defined 
 * struct. USB device expect exact numbers of bytes as sepcified in 
 * the command protocol. If passed less/more then the result is just 
 *  a stall on bulk pipe and is very hard to debug, given that stall 
 *  can means different things on different level of USB stack.  
 */



//FIXME: These individual command blocks can be replaced by a single 
//generic big block. But for sake of readability and initial prototyping 
//these are left are they are.   

#pragma pack(push, 1)

typedef struct read10_t {
    uint8_t cmd[10];
} read10_t;

typedef struct read12_t {
    uint8_t cmd[12];
} read12_t;


typedef struct read10_data_t {
    uint8_t data[512];
} read10_data_t;

typedef struct write10_t {
    uint8_t cmd[10];
} write10_t;

typedef struct sync_cache_t {
    uint8_t cmd[10];
} sync_cache_t;


typedef struct write10_data_t {
    uint8_t data[512];
} write10_data_t;

typedef struct inq_cmd_t {
    uint8_t op_code;
    uint8_t EVPD;
    uint8_t page_code;
    uint8_t MSB;
    uint8_t LSB;
    uint8_t ctrl;

} inq_cmd_t;

typedef struct read_capacity_t {
    uint8_t op_code;
    uint8_t rsvd;
    uint32_t lba;
    uint16_t rsvd1;
    uint8_t pmi;                // PMI is at LSB
    uint8_t ctrl;
} read_capacity_t;

typedef struct inq_data_t {
    uint8_t dev_type;
    uint8_t RMB;                // This resides in MSB
    uint8_t version;
    uint8_t resp_data_format;
    uint8_t additional_length;
    uint8_t rsvd[3];
    uint8_t vendor_info[8];
    uint8_t prod_id[16];
    uint8_t prod_revision[4];
} inq_data_t;

typedef struct read_capacity_data_t {
    uint8_t data[8];
} read_capacity_data_t;

#pragma pack()


/* For now assuming a flat device with direct 
 * addressable blocks
 * This struct can be changed as per requirements 
 */

typedef struct scsi_device_t {
    uint32_t block_last;
    uint32_t block_size;
    uint8_t usb_address;
    int valid;
    inq_data_t inq_data;

} scsi_device_t;


void print_inq_data(inq_data_t iq);

void get_inquiry_command(uint8_t len, inq_cmd_t * cmd);

void get_read_capacity_command(read_capacity_t * cmd);

void get_read10_command(read10_t * cmd, uint32_t block, uint16_t n,
                        uint8_t cache);

void get_read12_command(read12_t * cmd, uint32_t block, uint32_t n);

void get_sync_cache_command(sync_cache_t * cmd, uint32_t block, uint16_t n);

void get_write10_command(write10_t * cmd, uint32_t block, uint16_t n,
                         uint8_t cache);

int init_scsi_dev(read_capacity_data_t data, scsi_device_t * dev);

#endif                          // SCSI_H
