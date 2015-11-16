# Simple M5 script for use with Barrelfish.  This creates a very 
# basic machine with a set of CPUs connected directly to the
# simulated memory.  
#
# (See configs/example/* in the M5 distribution for examples of
# fuller scripts -- caches, etc.).

# Determine script pathname
import inspect, os
import optparse
import os
import sys
import m5

from m5.defines import buildEnv
from m5.objects import *
from m5.util import fatal

#from O3_ARM_v7a import *
import CacheConfig
from Caches import *

# Try to determine Barrelfish source directory
# We assume that this script remains in tools/arm_gem5
bfsrcdir='%s/../..' % os.path.dirname(inspect.getfile(inspect.currentframe()))
print "Barrelfish source-directory is assume to be %s" % bfsrcdir

class MemBus(SystemXBar):
    badaddr_responder = BadAddr()
    default = Self.badaddr_responder.pio
    
#######################################################################
#
# Utility functions
def setCPUClass(options):

    atomic = False
    if options.cpu_type == "timing":
        class TmpClass(TimingSimpleCPU): pass
    elif options.cpu_type == "detailed" or options.cpu_type == "arm_detailed":
        if not options.caches and not options.ruby:
            print "O3 CPU must be used with caches"
            sys.exit(1)
        if options.cpu_type == "arm_detailed":
            class TmpClass(O3_ARM_v7a_3): pass
        else:
            class TmpClass(DerivO3CPU): pass
    elif options.cpu_type == "inorder":
        if not options.caches:
            print "InOrder CPU must be used with caches"
            sys.exit(1)
        class TmpClass(InOrderCPU): pass
    else:
        class TmpClass(AtomicSimpleCPU): pass
        atomic = True

    CPUClass = None
    test_mem_mode = 'atomic'

    if not atomic:
            test_mem_mode = 'timing'

    return (TmpClass, test_mem_mode, CPUClass)

#######################################################################
#
# Check that we are running on a full-system arm simulator

if not buildEnv['TARGET_ISA'] == "arm":
    fatal("Expected TARGET_ISA == arm");
    
#######################################################################
#
# Set up basic configuration options 

parser = optparse.OptionParser()
parser.add_option("--kernel", action="store", type="string")
parser.add_option("--ramdisk", action="store", type="string") 
parser.add_option("-n", "--num_cpus", type="int", default=1)
parser.add_option("--cpu-type", type="choice", default="atomic",
                  choices = ["atomic", "arm_detailed"],
                  help = "type of cpu to run with")
parser.add_option("--caches", action="store_true")
parser.add_option("--l2cache", action="store_true")
parser.add_option("--l1d_size", type="string", default="64kB")
parser.add_option("--l1i_size", type="string", default="32kB")
parser.add_option("--l2_size", type="string", default="2MB")
parser.add_option("--l3_size", type="string", default="16MB")
parser.add_option("--l1d_assoc", type="int", default=2)
parser.add_option("--l1i_assoc", type="int", default=2)
parser.add_option("--l2_assoc", type="int", default=8)
parser.add_option("--l3_assoc", type="int", default=16)
parser.add_option("--cacheline_size", type="int", default=64)
parser.add_option("--loglevel", type="int", default=4)
parser.add_option("--clock", action="store", type="string", default='1GHz')
#parser.add_option("--memchecker", action="store_true")
#parser.add_option("-d", "--detailed", action="store_true")
parser.add_option("--spec-input", default="ref", type="choice",
                      choices=["ref", "test", "train", "smred", "mdred",
                               "lgred"],
                      help="Input set size for SPEC CPU2000 benchmarks.")
parser.add_option("--arm-iset", default="arm", type="choice",
                      choices=["arm", "thumb", "aarch64"],
                      help="ARM instruction set.")
(options, args) = parser.parse_args()
    
#######################################################################
#
# Create simulated machine.


(CPUClass, mem_mode, FutureClass) = setCPUClass(options)

system = ArmSystem()
#system = LinuxArmSystem()
#kernel to boot
system.kernel = options.kernel

#memory system
system.iobus = IOXBar()
#system.iobus = NoncoherentXBar()
#system.iobus.forward_latency = 0
#system.iobus.frontend_latency = 0
#system.iobus.response_latency = 0
system.membus = MemBus()
system.membus.badaddr_responder.warn_access = "warn"

system.bridge = Bridge(delay='50ns')
system.bridge.master = system.iobus.slave
system.bridge.slave = system.membus.master

system.physmem = SimpleMemory(range = AddrRange('256MB'),conf_table_reported = True)

system.mem_mode = mem_mode
#load ramdisk at specific location (256MB = @0x10000000)
#system.ramdisk = SimpleMemory(range = AddrRange(Addr('256MB'), size = '256MB'), file=options.ramdisk)
#system.ramdisk.port = system.membus.master

#CPU(s)
#CPUClass.clock = "1GHz"
system.cpu = [CPUClass(cpu_id=i) for i in xrange(options.num_cpus)]

#machine type
system.realview = VExpress_EMM()

#setup bootloader
system.realview.nvmem = SimpleMemory(range = AddrRange(Addr('2GB'), size = '64MB'), null = True)
system.realview.nvmem.port = system.membus.master
# System boot loader is now given relative to source directory
system.boot_loader = ('%s/tools/aarch64_gem5/boot_emm.aarch64' % bfsrcdir)
system.highest_el_is_64 = True
#system.boot_loader_mem = system.realview.nvmem
#system.realview.setupBootLoader(system.membus, system, '../tools/arm_gem5/boot.arm')
system.gic_cpu_addr = system.realview.gic.cpu_addr
system.flags_addr = system.realview.realview_io.pio_addr + 0x30

boot_flags = 'rw loglevel=%s' % (options.loglevel)
system.boot_osflags = boot_flags

system.realview.attachOnChipIO(system.membus, system.bridge)
system.realview.attachIO(system.iobus)
system.intrctrl = IntrControl()
system.terminal = Terminal()
system.vncserver = VncServer()

system.physmem.port = system.membus.master


system.system_port = system.membus.slave

#Caches
if options.caches or options.l2cache:
    system.iocache = IOCache(addr_ranges=[system.physmem.range])
    system.iocache.cpu_side = system.iobus.master
    system.iocache.mem_side = system.membus.slave
else:
    system.iobridge = Bridge(delay='50ns',
                               ranges = [system.physmem.range])
    system.iobridge.slave = system.iobus.master
    system.iobridge.master = system.membus.slave
    
CacheConfig.config_cache(options, system)

#######################################################################
#
# Start simulation

root = Root(full_system=True, system=system)
m5.instantiate()

print '..... STARTING SIMULATION'

exit_event = m5.simulate()
exit_cause = exit_event.getCause()

print 'Exiting @ tick %i because %s' % (m5.curTick(), exit_cause)
