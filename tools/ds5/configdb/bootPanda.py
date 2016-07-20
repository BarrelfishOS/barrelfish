from arm_ds.debugger_v1 import Debugger
from arm_ds.debugger_v1 import DebugException
import os

# The Pandaboard must be in reset *before* this script starts.

# The CPU driver is linked at this address
LINKADDRESS = 0

# The boot driver is relocated to this address
RAMBASE = 0x80000000

debugger = Debugger()
ec = debugger.getCurrentExecutionContext()

# Load the boot driver symbols
im= ec.getImageService()
im.loadSymbols('Barrelfish/armv7/sbin/boot_omap44xx', RAMBASE)

# Use a hardware breakpoint to wait until the boot driver is loaded
# and running.
bs= ec.getBreakpointService()
bs.setBreakpoint('boot', hw=True, temporary=True)

# Use usbboot to download the image
os.system('Barrelfish/tools/bin/usbboot Barrelfish/armv7_pandaboard_image')

# Wait until we hit that breakpoint
es = ec.getExecutionService()
es.waitForStop()

# The previous execution context is now invalid
ec = debugger.getCurrentExecutionContext()

# Get the CPU driver's final load address.
vs = ec.getVariableService()
kernel_start = vs.readValue('boot_arguments.cpu_driver_base')

offset = int(kernel_start) - LINKADDRESS

print "Kernel loaded at: %08x" % int(kernel_start), " linked at %08x" % LINKADDRESS, " offset %08x" % offset

# Add the CPU driver symbols, but don't replace the boot driver ones.
im.addSymbols('Barrelfish/armv7/sbin/cpu_omap44xx', offset)