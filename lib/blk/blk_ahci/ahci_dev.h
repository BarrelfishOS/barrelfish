/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AHCI_DEV_H_
#define AHCI_DEV_H_

#include <stdbool.h>
#include <errors/errno.h>
#include <dev/ahci_hba_dev.h>
#include <dev/ahci_port_dev.h>
#include <dev/ata_identify_dev.h>

void ahci_hba_reset(ahci_hba_t* controller);
void ahci_hba_irq_enable(ahci_hba_t* controller);
void ahci_hba_irq_disable(ahci_hba_t* controller);
uint32_t ahci_hba_get_num_ports(ahci_hba_t* controller);
errval_t ahci_hba_init(ahci_hba_t* controller);
uint32_t ahci_hba_get_command_slots(ahci_hba_t* controller);
bool ahci_port_is_implemented(ahci_hba_t* controller, size_t port);
uint32_t ahci_port_probe(ahci_port_t* port);

void ahci_port_start(ahci_port_t* port);
void ahci_port_stop(ahci_port_t* port);
bool ahci_port_is_running(ahci_port_t* port);
bool ahci_port_is_idle(ahci_port_t* port);
bool ahci_port_is_functional(ahci_port_t* port);
bool ahci_port_is_ready(ahci_port_t* port);
size_t ahci_port_offset(uint32_t port);
bool ahci_port_slot_free(ahci_port_t* port, uint8_t slot);

void ahci_port_print_identification(ata_identify_t *id);

#endif // AHCI_DEV_H_
