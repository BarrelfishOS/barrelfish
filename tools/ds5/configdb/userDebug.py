from arm_ds.debugger_v1 import Debugger
from arm_ds.debugger_v1 import DebugException
import os

# The CPU driver is linked at this address
LINKADDRESS = 0

debugger = Debugger()
ec = debugger.getCurrentExecutionContext()
es = ec.getExecutionService()

# Run until the end of molly, to discover where the kernel has been loaded.
# XXX - this is fragile, and should be replaced with a symbol.  
es.resumeTo('molly_init32.c', 111)
es.waitForStop()

# The old execution context became invalid when we resumed.
ec = debugger.getCurrentExecutionContext()

# Get the CPU driver's final load address.
vs = ec.getVariableService()
kernel_start = vs.readValue('kernel_start')

offset = int(kernel_start) - LINKADDRESS

print "Kernel loaded at: %08x" % int(kernel_start), " linked at %08x" % LINKADDRESS, " offset %08x" % offset

# Replace the molly symbols with the kernel symbols
im= ec.getImageService()
im.loadSymbols('Barrelfish/armv7/sbin/cpu_fvp', offset)

# Finally, advance to arch_init()
es.resumeTo('arch_init')
