from arm_ds.debugger_v1 import Debugger
from arm_ds.debugger_v1 import DebugException

f= file("armv8_fvp_debug")
details= {}

for line in f:
    k, v= line.strip().split("\t")
    details[k]= v

del f

debugger= Debugger()

ec= debugger.getExecutionContext(0)

# Load the image
ms= ec.getMemoryService()
ms.restore("armv8_fvp_image", "binary", details["load_address"])

# Load the shim symbols
image= ec.getImageService()
image.addSymbols("armv8/sbin/fvp_shim", details["shim_address"])

# Load the CPU driver symbols in their kernel-window location
image.addSymbols("armv8/sbin/cpu_foundation",
                 "EL1N:" + details["cpudriver_address"])

# Load the VM init symbols in their physical location
ec.executeDSCommand(
        "add-symbol-file armv8/kernel/arch/armv8/foundation/vminit.o " +
        "-s .vminit " + details["vminit_address"])

# Debug from the shim entry point
es= ec.getExecutionService()
es.setExecutionAddress(details["entry_point"])
