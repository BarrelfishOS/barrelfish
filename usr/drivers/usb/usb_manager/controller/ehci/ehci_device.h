/*
 * ehci_device.h
 *
 *  Created on: May 11, 2013
 *      Author: reto
 */

#ifndef EHCI_DEVICE_H_
#define EHCI_DEVICE_H_

#if defined(__x86_64__) || defined(__i386__)
#include <dev/ehci_dev.h>
#else
#include <dev/omap/ehci_dev.h>
#endif


#endif /* EHCI_DEVICE_H_ */
