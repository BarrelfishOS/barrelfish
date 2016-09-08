/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MCDI_RPC_H_
#define MCDI_RPC_H_

// MCDI Constant Offsets
#define MCDI_REBOOT_OFFSET(a) (a? 0x7fc/4 :0x7f8/4)
#define MCDI_PDU(a) (a? 0x108/4 : 0x008/4)
#define MCDI_DORBELL(a) (a? 0x004/4: 0x000)
#define MCDI_MAC_PORT_OFFSET(a) (a? 50 : 44)
// CMD for getting assertions
#define CMD_GET_ASSERTS 0x6
#define CMD_GET_ASSERTS_IN_LEN 4
#define CMD_GET_ASSERTS_IN_CLEAR_OFFSET 0
#define CMD_GET_ASSERTS_OUT_LEN 140
#define CMD_GET_ASSERTS_FLAG_THR_FAIL 0x2
#define CMD_GET_ASSERTS_FLAG_SYS_FAIL 0x3
#define CMD_GET_ASSERTS_FLAG_WDOG_FAIL 0x4
// CMD for Driver attaching
#define CMD_DRV_ATTACH 0x1c
#define CMD_DRV_ATTACH_IN_LEN 8
#define CMD_DRV_ATTACH_OUT_LEN 4
// CMD for Reboot
#define CMD_REBOOT 0x3d
#define CMD_REBOOT_IN_LEN 4
// CMD fetting firmware version
#define CMD_GET_VERSION 0x8
#define CMD_GET_VERSION_OUT_LEN 32
// CMD for Port reset
#define CMD_PORT_RESET 0x20
// CMD for WoL Filter get
#define CMD_WOL_FILTER_GET 0x45
#define CMD_WOL_FILTER_RESET 0x34
#define CMD_WOL_FILTER_GET_OUT_LEN 4
// CMD for getting Board config
#define CMD_GET_BOARD_CONFIG 0x18
#define CMD_GET_BOARD_CONFIG_OUT_LEN 136
#define CMD_GET_BOARD_CONFIG_OUT_CAPABILITIES(a) (a? 40: 36)
// CMD SRIOV
#define SRIOV_BASE 0x40
#define CMD_SRIOV 0x30
#define CMD_SRIOV_IN_LEN 12
#define CMD_SRIOV_OUT_LEN 8
// CMD PTP
#define CMD_PTP 0xb
#define CMD_PTP_IN_LEN 4
#define PTP_DISABLE 0x2
// CMD get resource limits
#define CMD_GET_RESOURCE_LIMITS 0x4f
#define CMD_GET_RESOURCE_LIMITS_OUT_LEN 16
// CMD for ports setup
#define CMD_GET_LINK 0x29
#define CMD_GET_LINK_OUT_LEN 28
#define CMD_GET_LINK_OUT_SPEED_OFFSET 8
#define CMD_GET_LINK_OUT_FLAGS_OFFSET 16
#define CMD_GET_LINK_OUT_FCNTL_OFFSET 20
#define CMD_GET_LINK_OUT_MAC_FAULT_OFFSET 24

#define CMD_GET_LOOPBACK_MODES 0x28
#define CMD_GET_LOOPBACK_MODES_OUT_LEN 32
#define CMD_GET_LOOPBACK_MODES_SUGGESTED_OFFSET 24
// CMD for setting link
#define CMD_SET_LINK 0x2a
#define CMD_SET_LINK_IN_LEN 16
#define CMD_SET_LINK_IN_CAP_OFFSET 0
#define CMD_SET_LINK_IN_FLAGS_OFFSET 4
#define CMD_SET_LINK_IN_LOOPBACK_MODE_OFFSET 8
#define CMD_SET_LINK_IN_LOOPBACK_SPEED_OFFSET 12
// CMD for enable logging
#define CMD_LOG_CTRL 0x7
#define CMD_LOG_CTRL_IN_LEN 8
// CMD for setting mac address
#define CMD_SET_MAC 0x2c
#define CMD_SET_MAC_IN_LEN 24
#define CMD_SET_MAC_IN_ADR_OFFSET 8
#define CMD_SET_MAC_IN_MTU_OFFSET 0
#define CMD_SET_MAC_IN_DRAIN_OFFSET 4
#define CMD_SET_MAC_IN_REJECT_OFFSET 16
#define CMD_SET_MAC_IN_FCTNL_OFFSET 20
// CMD for setting multicast hash
#define CMD_SET_MCAST_HASH 0x35
#define CMD_SET_MCAST_HASH_IN_LEN 32
#define CMD_SET_MCAST_IN_HASH0_OFFSET 0
#define CMD_SET_MCAST_IN_HASH1_OFFSET 16
// CMD for getting phy config
#define CMD_GET_PHY_CFG 0x24
#define CMD_GET_PHY_CFG_OUT_LEN 72
#define CMD_GET_PHY_CFG_OUT_FLAGS_OFFSET 0
#define CMD_GET_PHY_CFG_OUT_TYPE_OFFSET 4
#define CMD_GET_PHY_CFG_OUT_CAP_OFFSET 8
#define CMD_GET_PHY_CFG_OUT_MEDIA_OFFSET 44
// CMD for getting phy state out
#define CMD_GET_PHY_STATE 0x43
#define CMD_GET_PHY_STATE_OUT_LEN 4
// CMD for mon probe
#define CMD_SENSOR_INFO 0x41
#define CMD_SENSOR_INFO_OUT_LEN 252
#define CMD_SENSOR_INFO_OUT_MASK 0
// CMD for BIST test
#define CMD_START_BIST 0x25
#define CMD_START_BIST_IN_LEN 4
#define CMD_PHY_BIST 5
#define CMD_POLL_BIST 0x26
#define CMD_POLL_BIST_RUNNING 1
#define CMD_POLL_BIST_PASSED  2
#define CMD_POLL_BIST_FAILED  3
#define CMD_POLL_BIST_OUT_LEN 8
// CMD for MAC stats
#define CMD_MAC_STATS_IN_LEN 16
#define CMD_MAC_STATS 0x2e
#define CMD_MAC_STATS_IN_CMD_OFFSET 8
#define CMD_MAC_STATS_IN_ADDR_LO_OFFSET 0
#define CMD_MAC_STATS_IN_ADDR_HI_OFFSET 4
#define CMD_MAC_STATS_IN_DMA_LEN_OFFSET 12
// CMD for PHY stats
#define CMD_PHY_STATS 0x2d
#define CMD_PHY_STATS_IN_LEN 8
#define CMD_PHY_STATS_IN_ADDR_OFFSET 0
// CMD for info about virtual NVRAM partition
#define CMD_NVRAM_INFO 0x37
#define CMD_NVRAM_INFO_IN_LEN 4
#define CMD_NVRAM_INFO_IN_TYPE_OFFSET 0
#define CMD_NVRAM_INFO_OUT_LEN 24
//CMD 
#define CMD_NVRAM_TYPES 0x36
#define CMD_NVRAM_TYPES_OUT_LEN 4
errval_t mcdi_rpc(unsigned cmd, const uint8_t *in, uint32_t inlen,
             uint8_t *out, uint32_t outlen, uint32_t *outlen_actual,
             bool port, sfn5122f_t *d);
void init_mcdi_mutex(void);


#endif 


