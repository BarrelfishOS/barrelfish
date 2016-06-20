# this script implements the Java interface IOSProvider
from osapi import DebugSessionException, ExecutionContext, ExecutionContextsProvider, \
                  Table, createField, DECIMAL, TEXT, ADDRESS, Model

def areOSSymbolsLoaded(debugger):
    return debugger.symbolExists('context_switch_counter')

def isOSInitialised(debugger):
    try:
        # Once we've switched to the init dispatcher, we're initialised
        # enough to respond to the debugger.
        nswitches = debugger.evaluateExpression('context_switch_counter')
        return nswitches >= 1
    except DebugSessionException:
        return False

def getOSContextProvider():
    return ContextsProvider()

def getDataModel():
    return Model('Barrelfish', [RunnableDispatchers()])

def runnable_dispatchers(debugger):
    # Walk the current kernel's scheduling queue
    dcb_ptr = debugger.evaluateExpression('kcb_current->queue_head')
    while dcb_ptr.readAsNumber() != 0:
        yield dcb_ptr
        
        fields = dcb_ptr.dereferencePointer().getStructureMembers()
        dcb_ptr = fields['next']

class RunnableDispatchers(Table):
    def __init__(self):
        id = "dispatchers"
        fields = [createField(id, 'dcb', ADDRESS),
                  createField(id, 'name', TEXT),
                  createField(id, 'disabled', DECIMAL),
                  createField(id, 'domain', DECIMAL),
                  createField(id, 'udisp', ADDRESS),
                  createField(id, 'core', DECIMAL)]
        Table.__init__(self, id, fields)
        
    def getRecords(self, debugger):
        return []

def dispatcherContext(debugger, id, dcb_ptr):
    # Get the handle to the shared dispatcher structure
    dcb = dcb_ptr.dereferencePointer()
    fields = dcb.getStructureMembers()
    handle = fields['disp'].readAsNumber()
    
    # Cast and dereference the handle to find the ARM shared structure
    dispatcher_shared_arm = debugger.evaluateExpression('*((struct dispatcher_shared_arm *)0x%x)' % handle)
    dsa_fields = dispatcher_shared_arm.getStructureMembers()
    
    # Grab the generic shared fields
    dispatcher_shared_generic = dsa_fields['d']
    dsg_fields = dispatcher_shared_generic.getStructureMembers()
    
    name = dsg_fields['name'].readAsNullTerminatedString()
    disabled = dsg_fields['disabled'].readAsNumber()
    
    state = ""
    if disabled:
        state += "D"
    else:
        state += "E"
            
    ec = ExecutionContext(id, name, state)
    ec.getAdditionalData()['kernel'] = False
    
    return ec
    
def kernelContext(debugger, id, kcb_ptr):
    ec = ExecutionContext(id, 'kernel', None)
    ec.getAdditionalData()['kernel'] = True
    
    return ec

class ContextsProvider(ExecutionContextsProvider):
    def getCurrentOSContext(self, debugger):
        pc = debugger.evaluateExpression('$pc').readAsNumber()
        if pc >= 0x80000000:
            # We're in the kernel - use the KCB address as context ID
            kcb_current = debugger.evaluateExpression('kcb_current')
            id = kcb_current.readAsNumber()
            return kernelContext(debugger, id, kcb_current)
        else :
            # We use the physical address of the DCB to identify dispatchers.
            dcb_current = debugger.evaluateExpression('dcb_current')
            id = dcb_current.readAsNumber()
            if id == 0:
                return None
            else:
                return dispatcherContext(debugger, id, dcb_current)
        
    def getAllOSContexts(self, debugger):
        contexts = []
        
        for d in runnable_dispatchers(debugger):
            contexts.append(dispatcherContext(debugger, d.readAsNumber(), d))
            
        return contexts            
        
    def getOSContextSavedRegister(self, debugger, context, name):
        if context.getAdditionalData()['kernel']:
            return None
        
        # Reconstruct a pointer to the DCB from the context ID
        dcb = debugger.evaluateExpression('*((struct dcb *)0x%x)' % context.getId())        
        fields = dcb.getStructureMembers()
    
        # Cast and dereference the handle to find the ARM shared structure
        handle = fields['disp'].readAsNumber()
        dispatcher_shared_arm = debugger.evaluateExpression('*((struct dispatcher_shared_arm *)0x%x)' % handle)
        dsa_fields = dispatcher_shared_arm.getStructureMembers()
        
        # Look in the enabled or disabled area, as appropriate
        if fields['disabled'].readAsNumber() == 0:
            save_area = dsa_fields['enabled_save_area']
        else:
            save_area = dsa_fields['disabled_save_area']
            
        sa_fields = save_area.getStructureMembers() 
        regs = sa_fields['named'].getStructureMembers()
        
        # Map DS-5 register names to fields
        if name == "XPSR":
            return regs['cpsr']
        elif name == "R9":
            return regs['rtls']
        elif name == "SP":
            return regs['stack']
        elif name == "LR":
            return regs['link']
        elif name == "PC":
            return regs['pc']
        else:
            return regs[name.lower()]