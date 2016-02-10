#
# GAP Dominance detector as used in CP 2003 paper
#
#   Steve Linton
#
# $Id: SBDDa.g,v 1.2 2007/07/03 00:10:27 jschimpf Exp $
#

#
# Copyright (C) 2003-2004  The SBDS Group
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

#
#  The interface to this file is four functions: 
#      SBDDInit( g, n)   initialize the process with symmetry group g
#         acting on n points (1..n)
#      
#      BacktrackRoot( choice, depth ) informs the dominance detection
#           that the subtree at the top level headed by choice has
#          been exhausted

#       Backtrack( choice, d, node ) informs the dominance detection
#         that the subtree under node, headed by choice has been exhausted
#         
#         Note that actually this code only uses {choice} U node and
#         does not care about which decision is "highlighted" as choice
#         
#         CheckOptionList( set, optpt, listlists) asks the dominance detection
#         to check whether the choice optpt in the context set is
#         allowed. listlists is the complete list of domains, used at
#         the moment only to avoid returning deletions of points in
#         domains which are not still there.
#         
#

#
# Initialize global variables to avoid parser warnings
# These are set in Init
#
G := fail;               # symmetry group
treeroot := fail;        # main data structure 
N := fail;               # number of points
DominanceStats := fail;  # statistics


#
# declare some constants
#

#
# outcomes for the statistics
#
D_FALSE := 1;
D_TRUE_NODEL := 2;
D_TRUE_DEL := 3;
D_TRUE_ORBIT := 4;

#
# node types
#
N_POINT := 1;
N_FORK := 2;
N_FINAL := 3;
N_TERMINAL := 4;
N_BACKTRACK := 5;
N_PENDING := 6;

#
# Declare some useful stuff for diagnostic messages/tracing.
# InfoSBDD is normal, human-readable output.
# InfoSBDDtree is for printing the search tree as an ECLiPSe term (modulo
# the '#I's from the Info output) which can then be read in and visualised.
#
InfoSBDD := NewInfoClass("InfoSBDD");
InfoSBDDtree := NewInfoClass("InfoSBDDtree");
I_NONE := 0;
I_BASIC := 1;
I_TRACE := 2;

#
# Allow for various GAP versions
#
if not IsBound(OrbitsDomain) then
OrbitsDomain := Orbits;
fi;

#
# Utility functions called when needed to complete various parts of
# the data structure.
#

#
# Details of the orbit structure of the group associated with a node
# on 1..N, used to select the order in which to treat points along the
# spine of the tree
#
FillInOrbitDetails := function(node)
    local   i,  j;
    node.orbits := OrbitsDomain(node.group, [1..N]);
    node.norbs := Length(node.orbits);
    node.orbsizes := List(node.orbits,Length);
    node.orbmap := [];
    for i in [1..node.norbs] do
        for j in node.orbits[i] do
            node.orbmap[j] := i;
        od;
    od;
end;

#
# A final node "stores" the points from the most recent failset. We
# don't know until we get a query which points are on the spine and
#                 which on the branch
#  This function is called when we find out and converts the final
#  node into a suitable combination of pending and fork nodes

ExpandFinalNode := function(node, set)
    local   spinal;
    # turn it into a possible pending node and a fork node and another
    # pending node
    #
    spinal := Intersection(node.set, set);
    if Length(spinal) = 0 then
        node.type := N_FORK;
        node.left :=  rec( type := N_PENDING,
                           set := node.set,
                           parent := node,
                           child := rec(type := N_BACKTRACK));
        node.spinetohere := node.parent.spinetohere;
        node.left.child.parent := node.left;
        Unbind(node.set);
    else
        node.type := N_PENDING;
        node.child := rec( type := N_FORK,
                           parent := node,
                           depth := node.depth,
                           left := rec( type := N_PENDING,
                                   set := Difference(node.set, set),
                                   child := rec(type := N_BACKTRACK)),
                           touchedorbits := node.touchedorbits,
                           lastOK := node.lastOK,
                           spinetohere := Union(node.parent.spinetohere, spinal));
        node.child.left.parent := node.child;
        node.child.left.child.parent := node.child.left;
        treeroot.bottom := node.child;
        node.set := spinal;
        Unbind(node.depth);
        Unbind(node.touchedorbits);
        Unbind(node.lastOK);
    fi;
end;


#
# Forward declaration for complex recursion
#
ExpandPendingNode1 := fail;

#
# A point node contains the Schreier vector and other data corresponding to a
# specific point that we will try to map into the current choice set
#
# This function builds that data, once a point has been chosen. In
# fact it turns a Pending node into a Point node and creates a new
# Pending node below it (which it recursively expands. The paramater below
# is the node below this one, to which we must link any new nodes
#

ExpandPointNode := function(node,below,point)
    local   sv,  q,  gens,  igens,  ngens,  i,  img, pt;
    node.pt := point;
    node.type := N_POINT;
    sv := [];
    sv[point] := 0;
    q := [point];
    gens := GeneratorsOfGroup(node.group);
    igens := List(gens,Inverse);
    node.igens := igens;
    node.gens := gens;
    ngens := Length(gens);
    for pt in q do
        for i in [1..ngens] do
            img := pt^gens[i];
            if not IsBound(sv[img]) then
                Add(q,img);
                sv[img] := i;
            fi;
        od;
    od;
    node.schreier := sv;
    if Length(node.set) = 1 then
        node.child := below;
        below.group := Stabilizer(node.group, point);
        below.parent := node;
    else
        node.child := rec(type := N_PENDING,
                          set := Difference(node.set,[point]),
                          parent := node,
                          group := Stabilizer(node.group,point));
        ExpandPendingNode1(node.child, below);
    fi;
    Unbind(node.set);
end;

#
# when there is no subgroup left, and we are in a left branch, we can 
# use a terminal node and more efficient processing
#

ExpandTerminalNode := function(node,below)
    node.type := N_TERMINAL;
    node.child := below;
    below.parent := node;
    below.group := node.group;
end;

#
# Here we choose a point to treat first and expand it.
#

ExpandPendingNode1 := function(node, below)
    local   i,  best,  bestpt;
    if below.type = N_BACKTRACK and
       ForAll(GeneratorsOfGroup(node.group), g->OnSets(node.set,g) =
              node.set) then 
        ExpandTerminalNode(node,below);
    else
        FillInOrbitDetails(node);
        if Length(node.set) = 1 then
            ExpandPointNode(node, below,node.set[1]);
        else
            best := infinity;
            bestpt := fail;
            for i in node.set do
                if node.orbsizes[node.orbmap[i]] < best then
                    best := node.orbsizes[node.orbmap[i]];
                    bestpt := i;
                fi;
            od;
            ExpandPointNode(node,below, bestpt);
        fi;
    fi;
end;



#
# A bit of admin, and then enter the recursive expansion code above.
#

ExpandPendingNode := function(node)
    local   fixGroup;
    fixGroup := function(node)
        if not IsBound(node.group) then
            Assert(1,node.parent.type = N_FORK);
            node.group := fixGroup(node.parent);
        fi;
        return node.group;
    end;
    fixGroup(node);
    ExpandPendingNode1(node, node.child);
end;

#
# The public initialization function
#

SBDDInit := function(g, n)
    G := g;
    N := n;
    treeroot := rec(type:=N_FORK, 
                    depth := -1, 
                    spinetohere := [],
                    group := G,
                    touchedorbits := [],
                    lastOK:= [] );
    treeroot.bottom := treeroot;
    DominanceStats := [0,0,0,0];
    FillInOrbitDetails(treeroot);
end;

#
# Hacky rubbish to make indenting Info output based on a "depth" more
# convenient.  D is a function which, given an argument, returns a blank
# string of that length; given no argument, it returns the same string as
# last time.
#

LastDepth := 0;
LastDepthString := "";
D := function(arg)
    if IsBound(arg[1]) then
	LastDepth := arg[1];
	LastDepthString := String("", arg[1]);
    fi;
    return LastDepthString;
end;

#
# More hackery, for recording the tranformation mappings applied to points
# during an iSearch(), so that when printing a trace of the search we can
# map them back to their original values.  The slightly odd way of doing
# this (embedded in a function returning the empty string) is so that it can
# be embedded in a call to Info() and thus only done if we actually need it.
# (Maybe there's a better way to do this?)
#

MappingInfo := [];
RecordMappingInfo := function(depth0, gens, l)
    local   depth;

    depth := depth0 + 1;
    #Info(InfoSBDDtree, I_TRACE, "Depth:", depth);
    MappingInfo[depth] := rec(gens := gens, l := l);

    return "";  #  :)
end;

ResetMappingDepth := function(depth0)
    local   depth, d, i;

    depth := depth0 + 1;
    #Info(InfoSBDDtree, I_TRACE, "Depth:", depth);
    # We've backtracked, so clear out the old data.
    d := Length(MappingInfo);
    for i in [depth .. d] do
	Unbind(MappingInfo[i]);
    od;

    return "";  #  :)
end;

# Use the recorded mapping info to map a point back to its original.
MapPoint := function(point)
    local   i, d, gens, l;

    for d in [Length(MappingInfo),Length(MappingInfo)-1..1] do
	gens := MappingInfo[d].gens;
	l    := MappingInfo[d].l;
	for i in [Length(l),Length(l)-1..1] do
	    point := point^gens[l[i]];
	od;
    od;
    return point;
end;

# Use the recorded mapping info to map a tuple back to its original.
MapTuple := function(tuple)
    local   i, d, gens, l;

    for d in [Length(MappingInfo),Length(MappingInfo)-1..1] do
	gens := MappingInfo[d].gens;
	l    := MappingInfo[d].l;
	for i in [Length(l),Length(l)-1..1] do
	    tuple := OnTuples(tuple, gens[l[i]]);
	od;
    od;
    return tuple;
end;


#
# The main worker function. The recursive search through the tree.
#
# node is the current node, Pdash the remaining points in the choice
# being checked for dominance, possibly mapped by a permutation so far
# xset if the original choice set, not mapped and forbid is a mapped list
# of points which should NOT be reported as domain deletions.
#

iSearch := function(node, Pdash, forbid, xset)
    local   toremove,  res,  x,  bottom,  gens,  sv,  igens,  
            y,  Q,  nforbid,  l,  a,  removals,  i, search_depth, first;
    
    Info(InfoSBDD, I_TRACE, D(Size(xset) - Size(Pdash)), "iSearch called");
    # Cache depth value...  Only used when tracing.
    search_depth := LastDepth;

    toremove := [];
    if node.type = N_FINAL then
        ExpandFinalNode(node, xset);
        if node.type = N_FORK then
            node.group := node.parent.group;
        fi;
    fi;
    if node.type = N_PENDING then
        ExpandPendingNode( node );
    fi;
    if node.type = N_FORK then
	Info(InfoSBDDtree, I_TRACE, "fork(");
        if IsBound(node.left) then
            res := iSearch(node.left, Pdash, forbid, xset);
            if res = false then
		Info(InfoSBDDtree, I_TRACE, ", skipped)");
                return false;
            fi;
            toremove := res;
            UniteSet(forbid, toremove);
	else
	    Info(InfoSBDDtree, I_TRACE, "not_bound");
        fi;
	Info(InfoSBDDtree, I_TRACE, ",");
        if IsBound(node.right) then
            res := iSearch(node.right, Pdash, forbid, xset);
	    Info(InfoSBDDtree, I_TRACE, ")");
            if res = false then
                return false;
            else
                UniteSet(toremove, res);
            fi;
	else
	    Info(InfoSBDDtree, I_TRACE, "not_bound)");
        fi;
        return toremove;
    fi;
    if node.type = N_BACKTRACK then
	Info(InfoSBDDtree, I_TRACE, "backtrack");
        return false;
    fi;
    #
    # handle the mathematical nodes
    #
    if node.type = N_TERMINAL then
	Info(InfoSBDD, I_TRACE, D(), "Trying to map terminal node.");
	Info(InfoSBDD, I_TRACE, D(), "set:   ", node.set);
	Info(InfoSBDD, I_TRACE, D(), "Pdash: ", Pdash);
	Info(InfoSBDDtree, I_TRACE, "set(", node.set, " / ",
			MapTuple(node.set), ", ");
        x := Difference(node.set,Pdash);
        if Length(x) = 0 then
	    Info(InfoSBDD, I_TRACE, D(), "Success.");
	    Info(InfoSBDDtree, I_TRACE, "dominated)");
            return false;
        elif Length(x) = 1 and not x[1] in forbid then 
	    Info(InfoSBDD, I_TRACE, D(), "Not quite: excluding ", x[1]);
	    Info(InfoSBDDtree, I_TRACE, "del)");
            return x;
        else
	    Info(InfoSBDD, I_TRACE, D(), "Failed.");
	    Info(InfoSBDDtree, I_TRACE, "nodel)");
            return [];
        fi;
    elif node.type = N_POINT then
	Info(InfoSBDD, I_TRACE, D(), "Trying to map point: ", node.pt);
	Info(InfoSBDD, I_TRACE, D(), "Orbit: ", node.orbits[node.orbmap[node.pt]]);
	Info(InfoSBDD, I_TRACE, D(), "Pdash: ", Pdash);
	Info(InfoSBDDtree, I_TRACE, "point(", node.pt, ", [");

        bottom := node.child.type = N_BACKTRACK;
        toremove := [];
        gens := node.gens;
        sv := node.schreier;
        igens := node.igens;
	first := true;		# Only used when tracing.
        for x in Pdash do 
            if IsBound(sv[x]) then
		Info(InfoSBDD, I_TRACE, D(search_depth), "Trying ", x);
		if first <> true then
		    Info(InfoSBDDtree, I_TRACE, ",");
		else
		    first := false;
		fi;
		Info(InfoSBDDtree, I_TRACE, x, "/", MapPoint(x), ":");
                y := x; 
                Q := ShallowCopy(Pdash);
                RemoveSet(Q,y);
                nforbid := forbid;
                l := [];
                a := sv[y];
                while 0 <> a do
                    Add(l,a);
                    x := igens[a];
                    Q := OnTuples(Q,x);
                    nforbid :=OnTuples(nforbid, x);
                    y := y^x;
                    a := sv[y];
                od;
                Sort(Q);
                Sort(nforbid);
		# Hack to record the mapping info, but only if we're
		# tracing...  Shame about the blank Info line it produces.
		# Note that this has to be called before the iSearch call...  :)
		Info(InfoSBDDtree, I_TRACE,
			RecordMappingInfo(Size(xset) - Size(Pdash), gens, l));
                res := iSearch(node.child, Q, nforbid, xset);
		# ... and the depth reset upon return.
		Info(InfoSBDDtree, I_TRACE,
			ResetMappingDepth(Size(xset) - Size(Pdash)));
                if res = false then
		    Info(InfoSBDDtree, I_TRACE, "])");
                    return false;
                fi;
                removals := res;
                if Length(removals) > 0 then
                    for i in [Length(l),Length(l)-1..1] do
                        removals := OnTuples(removals, gens[l[i]]);
                    od;
                    Sort(removals);
                    UniteSet(toremove, removals);
                    UniteSet(forbid, removals);
                fi;
            fi;                
        od;
	Info(InfoSBDD, I_TRACE, D(search_depth), "Search failed.");
	Info(InfoSBDDtree, I_TRACE, "])");
        if bottom then
            for x in [1..Length(node.schreier)] do
                if IsBound(node.schreier[x]) and not x in Pdash
                   and not x in forbid then
                    Add(toremove,x);
                fi;
            od;
            Sort(toremove);
        fi;
	Info(InfoSBDD, I_TRACE, D(), "Points to remove: ",
		toremove);
        return toremove;
    fi;
    Error("run off end of isearch");

end;

#
# Print the SBDD dominance tree data structure (the one that gets traversed
# by iSearch() above).
#
PrintTree := function(node)
    if node.type = N_FINAL then
	Info(InfoSBDDtree, I_TRACE, "final(", node.set, ")");
    fi;
    if node.type = N_PENDING then
        ExpandPendingNode( node );
    fi;
    if node.type = N_FORK then
	Info(InfoSBDDtree, I_TRACE, "fork(");
        if IsBound(node.left) then
            PrintTree(node.left);
	else
	    Info(InfoSBDDtree, I_TRACE, "not_bound");
        fi;
	Info(InfoSBDDtree, I_TRACE, ",");
        if IsBound(node.right) then
            PrintTree(node.right);
	    Info(InfoSBDDtree, I_TRACE, ")");
	else
	    Info(InfoSBDDtree, I_TRACE, "not_bound)");
        fi;
    fi;
    if node.type = N_BACKTRACK then
	Info(InfoSBDDtree, I_TRACE, "backtrack");
    fi;
    #
    # handle the mathematical nodes
    #
    if node.type = N_TERMINAL then
	Info(InfoSBDD, I_TRACE, D(), "Terminal node.");
	Info(InfoSBDD, I_TRACE, D(), "set:   ", node.set);
	Info(InfoSBDDtree, I_TRACE, "set(", node.set, ")");
    elif node.type = N_POINT then
	Info(InfoSBDD, I_TRACE, D(), "Point: ", node.pt);
	Info(InfoSBDD, I_TRACE, D(), "Orbit: ", node.orbits[node.orbmap[node.pt]]);
	Info(InfoSBDDtree, I_TRACE, "point(", node.pt, ", ");
	Info(InfoSBDDtree, I_TRACE, node.orbits[node.orbmap[node.pt]], ", ");
	PrintTree(node.child);
	Info(InfoSBDDtree, I_TRACE, ")");
    fi;
end;


MaintainOptionList := function(set, optpt, listlists)
    DominanceStats[4] := DominanceStats[4] + 1;
    
end;


# Used for attributing trace data to a particular call to CheckOptionList().
CheckOptionListCallCount := 0;

CheckOptionList := function( set, optpt, listlists)
    local   timeinit,  xset,  om,  to,  new,  forbid,  res;
    CheckOptionListCallCount := CheckOptionListCallCount + 1;
    Info(InfoSBDD, I_BASIC, "CheckOptionList called (",
	    CheckOptionListCallCount, ")");
    Info(InfoSBDDtree, I_TRACE, CheckOptionListCallCount, ":");
    timeinit := Runtime();
    xset := ShallowCopy(set);
    Add(xset, optpt);
    Sort(xset);
    om := treeroot.orbmap;
    to := treeroot.bottom.touchedorbits;
    xset := Filtered(xset, x->om[x] in to);
    new := Difference(xset, treeroot.bottom.lastOK);
    if Length(new) = 0 then
        DominanceStats[D_TRUE_ORBIT] := DominanceStats[D_TRUE_ORBIT]+1;
	Info(InfoSBDD, I_BASIC, "CheckOptionList done (orbit - ",
		Runtime() - timeinit, "ms)");
	Info(InfoSBDDtree, I_TRACE, "orbit.");
        return [true];
    fi;
    forbid := Difference([1..N], Set(Flat(listlists)));
    res := iSearch(treeroot, xset, forbid, xset);
    Info(InfoSBDDtree, I_TRACE, ".");
    # Print the dominance tree data structure that was just searched.
    Info(InfoSBDDtree, I_TRACE, CheckOptionListCallCount, ":");
    PrintTree(treeroot);
    Info(InfoSBDDtree, I_TRACE, ".");
    # Print the state that we tried to map the nogoods into.
    Info(InfoSBDDtree, I_TRACE, CheckOptionListCallCount, ":xset(", xset, ").");
    if res = false then 
        DominanceStats[D_FALSE] := DominanceStats[D_FALSE] + 1;
	Info(InfoSBDD, I_BASIC, "CheckOptionList done (dominated - ",
		Runtime() - timeinit, "ms)");
        return false;
    else
        treeroot.bottom.lastOK := xset;
        if Length(res) > 0 then
            DominanceStats[D_TRUE_DEL] := DominanceStats[D_TRUE_DEL] + 1;
	    Info(InfoSBDD, I_BASIC, "CheckOptionList done (del - ",
		    Runtime() - timeinit, "ms)");
        else
            DominanceStats[D_TRUE_NODEL] := DominanceStats[D_TRUE_NODEL] + 1;
	    Info(InfoSBDD, I_BASIC, "CheckOptionList done (nodel - ",
		    Runtime() - timeinit, "ms)");
        fi;
        return Concatenation([true],res);
    fi;
end;

RecordBacktrack := function( choice, d, nodeset)
    local   failset,  r,  pr;
    failset := ShallowCopy(nodeset);
    AddSet(failset, choice);
    #
    # find the fork or final node to which we will attach
    #
    r := treeroot;
    while r.depth < d  do
        pr := r;
        if not IsBound(r.right) then
            break;
        fi;
        r := r.right;
        while r.type <> N_FORK and r.type <> N_FINAL do
            r := r.child;
        od;
    od;
    #
    # So we want to attach ourselves to pr
    #
    if  pr.type = N_FINAL then
        ExpandFinalNode(pr, failset);
        if pr.type = N_PENDING then
            pr := pr.child;
        fi;
    fi;
    pr.right :=  rec( type := N_FINAL,
                      depth := d,
                      parent := pr,
                      touchedorbits := Union(pr.touchedorbits,Set(List(failset, x->treeroot.orbmap[x]))),
                      lastOK := [],
                      set := Difference(failset, pr.spinetohere));
    
    treeroot.bottom := pr.right;
end;

BacktrackRoot  := function( choice, d)
    Info(InfoSBDD, I_BASIC, D(d), "BacktrackRoot called");
    RecordBacktrack(choice[1],d,[]);
    Info(InfoSBDD, I_BASIC, D(d), "BacktrackRoot finished");
end;

Backtrack := function( choice, d, node)
    Info(InfoSBDD, I_BASIC, D(d), "Backtrack called");
    RecordBacktrack(choice,d,Set(node));
    Info(InfoSBDD, I_BASIC, D(d), "Backtrack finished");
end;


