# Simple M5 script for use with Barrelfish.  This creates a very 
# basic machine with a set of CPUs connected directly to the
# simulated memory.  
#
# (See configs/example/* in the M5 distribution for examples of
# fuller scripts -- caches, etc.).

import optparse
import os
import sys
import m5

from m5.defines import buildEnv
from m5.objects import *
from m5.util import fatal

from O3_ARM_v7a import *
from FSConfig import MemBus

#######################################################################
#
# Check that we are running on a full-system arm simulator

if not buildEnv['TARGET_ISA'] == "arm":
    fatal("Expected TARGET_ISA == arm");
    
#######################################################################
#
# Set up basic configuration options: kernel location and number of 
# CPUs. 

parser = optparse.OptionParser()
parser.add_option("--kernel", action="store", type="string")
parser.add_option("--ramdisk", action="store", type="string") 
#parser.add_option("--num_cpus", action="store", type="int")
(options, args) = parser.parse_args()
    
#######################################################################
#
# Create simulated machine.


CPUClass = O3_ARM_v7a_3
mem_mode = 'atomic'

system = LinuxArmSystem()

#kernel to boot
system.kernel = options.kernel


#memory system
system.iobus = Bus(bus_id=0)
system.membus = MemBus(bus_id=1)
system.membus.badaddr_responder.warn_access = "warn"

system.bridge = Bridge(delay='50ns', nack_delay='4ns')
system.bridge.master = system.iobus.slave
system.bridge.slave = system.membus.master

system.physmem = PhysicalMemory(range = AddrRange('128MB'))

#system.physmem = PhysicalMemory(range = AddrRange(Addr('128MB'), size = '128MB'), file=options.ramdisk)

system.mem_mode = mem_mode
#load ramdisk at specific location (128MB = @0x8000000)
system.ramdisk = PhysicalMemory(range = AddrRange(Addr('128MB'), size = '128MB'), file=options.ramdisk)
system.ramdisk.port = system.membus.master

#CPU(s)
CPUClass.clock = "1GHz"
system.cpu = CPUClass(cpu_id=0)

#machine type
system.machine_type = "RealView_PBX"
system.realview = RealViewPBX()

#setup bootloader
system.realview.nvmem = PhysicalMemory(range = AddrRange(Addr('512MB'), size = '64MB'), zero = True)
system.realview.nvmem.port = system.membus.master
system.boot_loader = '../tools/gem5/boot.arm'
system.boot_loader_mem = system.realview.nvmem
#system.realview.setupBootLoader(system.membus, system, binary)
system.gic_cpu_addr = system.realview.gic.cpu_addr
system.flags_addr = system.realview.realview_io.pio_addr + 0x30

system.realview.attachOnChipIO(system.membus, system.bridge)
system.realview.attachIO(system.iobus)
system.intrctrl = IntrControl()
system.terminal = Terminal()
system.vncserver = VncServer()

system.physmem.port = system.membus.master


system.system_port = system.membus.slave

#######################################################################
#
# Start simulation

root = Root(full_system=True, system=system)
m5.instantiate()

print '..... STARTING SIMULATION'

exit_event = m5.simulate()
exit_cause = exit_event.getCause()

print 'Exiting @ tick %i because %s' % (m5.curTick(), exit_cause)
