import re

RANGE_START=0x3ffe0000
RANGE_END=0x40000000

kaddr_cap_regex = re.compile(r"^(0x[0-9a-f]+){(.*)}$")
cap_regex = \
re.compile(r"^left=(?P<leftval>0x[0-9a-f]+),right=(?P<rightval>0x[0-9a-f]+),end=(?P<end>0x[0-9a-f]+),end_root=(?P<end_root>\d+),level=(?P<level>\d+),address=(?P<address>0x[0-9a-f]+),size=(?P<size>0x[0-9a-f]+),type=(?P<type>\d+),remote_rels=(?P<rcopies>\d)(?P<rancs>\d)(?P<rdescs>\d),extra=(?P<extra>.*)$")
typemap = {
 45:"ObjType_IPI",
 44:"ObjType_KernelControlBlock",
 43:"ObjType_PerfMon",
 42:"ObjType_ID",
 41:"ObjType_Notify_IPI",
 40:"ObjType_Notify_RCK",
 39:"ObjType_IO",
 38:"ObjType_IRQSrc",
 37:"ObjType_IRQDest",
 36:"ObjType_IRQTable",
 35:"ObjType_VNode_AARCH64_l3_Mapping",
 34:"ObjType_VNode_AARCH64_l3",
 33:"ObjType_VNode_AARCH64_l2_Mapping",
 32:"ObjType_VNode_AARCH64_l2",
 31:"ObjType_VNode_AARCH64_l1_Mapping",
 30:"ObjType_VNode_AARCH64_l1",
 29:"ObjType_VNode_ARM_l2_Mapping",
 28:"ObjType_VNode_ARM_l2",
 27:"ObjType_VNode_ARM_l1_Mapping",
 26:"ObjType_VNode_ARM_l1",
 25:"ObjType_VNode_x86_32_ptable_Mapping",
 24:"ObjType_VNode_x86_32_ptable",
 23:"ObjType_VNode_x86_32_pdir_Mapping",
 22:"ObjType_VNode_x86_32_pdir",
 21:"ObjType_VNode_x86_32_pdpt_Mapping",
 20:"ObjType_VNode_x86_32_pdpt",
 19:"ObjType_VNode_x86_64_ptable_Mapping",
 18:"ObjType_VNode_x86_64_ptable",
 17:"ObjType_VNode_x86_64_pdir_Mapping",
 16:"ObjType_VNode_x86_64_pdir",
 15:"ObjType_VNode_x86_64_pdpt_Mapping",
 14:"ObjType_VNode_x86_64_pdpt",
 13:"ObjType_VNode_x86_64_pml4_Mapping",
 12:"ObjType_VNode_x86_64_pml4",
 11:"ObjType_Kernel",
 10:"ObjType_DevFrame_Mapping",
 9 :"ObjType_DevFrame",
 8 :"ObjType_Frame_Mapping",
 7 :"ObjType_Frame",
 6 :"ObjType_EndPoint",
 5 :"ObjType_Dispatcher",
 4 :"ObjType_FCNode",
 3 :"ObjType_CNode",
 2 :"ObjType_RAM",
 1 :"ObjType_PhysAddr",
 0 :"ObjType_Null",
 }

class Capability(object):
    """Representation of a MDB node"""
    def __init__(self, capstring):
        capmatch = cap_regex.match(capstring)
        for key, value in capmatch.groupdict().items():
            val = value
            if key != "extra":
                val = int(value, 0)
            setattr(self, key, val)

        self.parent = None
        self.leftcap = None
        self.rightcap = None
        self.nodeid = -1

    def __str__(self):
        return "{address=0x%x, size=0x%x, type=%s, left=0x%x, right=0x%x}" % \
                (self.address, self.size, typemap[self.type], self.leftval, self.rightval)

    def set_parent(self, parentcap):
        self.parent = parentcap

    def set_left(self, leftcap):
        self.leftcap = leftcap

    def set_right(self, rightcap):
        self.rightcap = rightcap

    def set_nodeid(self, nodeid):
        self.nodeid = nodeid

def parse_file(fname):
    # nodes is map of kernel addr to cap
    nodes = {}
    with open(fname, 'r') as f:
        for l in f:
            l = l.strip()
            match = kaddr_cap_regex.match(l)
            kaddr, cap = match.groups()
            nodes[int(kaddr, 0)] = Capability(cap)
    return nodes

def build_tree(nodedict):
    for kaddr,cap in nodedict.items():
        left = cap.leftval
        right = cap.rightval
        if left != 0:
            leftcap = nodedict[left]
            leftcap.set_parent(cap)
        else:
            leftcap = None
        if right != 0:
            rightcap = nodedict[right]
            rightcap.set_parent(cap)
        else:
            rightcap = None

        cap.set_left(leftcap)
        cap.set_right(rightcap)

    root = None
    for kaddr,cap in nodedict.items():
        if cap.parent is None:
            root = cap

    return root

def write_tree(root, outfh):
    mynodeid = root.nodeid
    if root.leftcap is not None:
        leftid = root.leftcap.nodeid
        outfh.write("  n%d -- n%d\n" % (mynodeid, leftid))
        write_tree(root.leftcap, outfh)
    if root.rightcap is not None:
        rightid=  root.rightcap.nodeid
        outfh.write("  n%d -- n%d\n" % (mynodeid, rightid))
        write_tree(root.rightcap, outfh)

def write_dot_file(nodedict, treeroot, outfname):
    nodeid = 0
    with open(outfname, "w") as f:
        f.write("graph mdb {\n")
        # generate nodes
        f.write("  // list of all nodes\n")
        for kaddr,cap in nodedict.items():
            color = "black"
            cstart = cap.address
            cend = cap.address + cap.size
            if cstart >= RANGE_START and cend <= RANGE_END:
                # cap completely in target range
                print "cap inside target range"
                color = "red"
            elif cend >= RANGE_START and cend <= RANGE_END:
                # cap ends in target range
                print "cap ends inside target range"
                color = "blue"
            elif cstart >= RANGE_START and cstart <= RANGE_END:
                # cap starts in target range
                print "cap starts inside target range"
                color = "green"
            f.write("  n%d [label=\"0x%x--0x%x [%d]\",color=\"%s\"];\n" % \
                    (nodeid, cap.address,cap.address + cap.size, cap.type, color))
            cap.set_nodeid(nodeid)
            nodeid += 1

        f.write("  // Tree\n")
        write_tree(treeroot, f)
        f.write("}\n")



if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print "usage %s mdb_dump.txt [output.dot]" % sys.argv[0]
        sys.exit(1)

    nodes = parse_file(sys.argv[1])
    treeroot = build_tree(nodes)

    outf = "output.dot"
    if len(sys.argv) >= 3:
        outf = sys.argv[2]
    write_dot_file(nodes, treeroot, outf)

    sys.exit(0)
