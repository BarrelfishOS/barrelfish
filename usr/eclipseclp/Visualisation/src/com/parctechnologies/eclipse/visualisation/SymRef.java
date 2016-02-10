// BEGIN LICENSE BLOCK
// Version: CMPL 1.1
//
// The contents of this file are subject to the Cisco-style Mozilla Public
// License Version 1.1 (the "License"); you may not use this file except
// in compliance with the License.  You may obtain a copy of the License
// at www.eclipse-clp.org/license.
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
// the License for the specific language governing rights and limitations
// under the License. 
// 
// The Original Code is  The ECLiPSe Constraint Logic Programming System. 
// The Initial Developer of the Original Code is  Cisco Systems, Inc. 
// Portions created by the Initial Developer are
// Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): 
// 
// END LICENSE BLOCK

package com.parctechnologies.eclipse.visualisation;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.io.Serializable;
import java.lang.ref.*;

/**
 * A classd designed to act as a Symbolic Reference to objects.
 *
 * Intended primarily to be used in the recording and playback of commands.
 *
 */
public class SymRef implements Serializable {
    /** defines the root of the symbolic heirarchy */
    public static final SymRef ROOT = new SymRef();

    protected List path;

    /**
     * Private constructor for the ROOT SymRef
     */
    private SymRef() {
	this.path = new LinkedList();
	this.path.add("ROOT");
    }


    /**
     * Private constructor to turn a path into a SymRef
     */
    private SymRef(List path) {
	this.path = path;
    }

    /**
     * Holds the root of the tree
     */
    private static Node root = new Node();


    /**
     * Constructs a top level SymRef.
     * This is shorthand for calling
     * <CODE>SymRef(value, SymRef.ROOT, childName)</CODE>
     * @param value The object to be refered to by this SymRef
     * @param childName An object identifying the child
     */
    public SymRef(SymRefable value, Object childName) {
	this.path = Node.put(childName, value);
    }

    /**
     * Constructs a child SymRef
     * @param value The object to be refered to by this SymRef
     * @param parentRef The SymRef of the parent object (if applicable)
     * @param childName An object identifying the child
     */
    public SymRef(SymRefable value, SymRef parentRef, Object childName) {
	this.path = Node.addChild(parentRef.path, childName, value);
    }


    /**
     * @return The SymRef of the root top level object
     */
    public SymRef getRoot() {
	// make a new path with just the top most element of this symref
	List newPath = new LinkedList();
	newPath.add(this.path.get(0));
	return new SymRef(newPath);
    }


    /**
     * Returns the string representation of the symRef
     */
    public String toString() {
	StringBuffer sb = new StringBuffer();
	for(Iterator it = path.iterator(); it.hasNext(); ) {
	    sb.append("/").append(it.next());
	}
	return sb.toString();
    }

    /**
     * Handles the equality testing of symbolic keys
     */
    public boolean equals(Object o) {
	if (o instanceof SymRef) {
	    return toString().equals(((SymRef)o).toString());
	} else {
	    return false;
	}
    }

    /**
     * Handles the hashing of SymRefs
     */
    public int hashCode() {
	return toString().hashCode();
    }

    /**
     * Looks up the value of a SymRef
     */
    public static synchronized SymRefable get(SymRef key) throws InvalidSymRefException {
	if ( Node.containsKey(key.path) ) {
	    SymRefable value = (Node.findNode(key.path).get());
	    if (value != null) {
		return value;
	    } else {
		if (DebuggingSupport.logMessages) {
		    DebuggingSupport.
			logMessage(key,
				   "SymRef has been garbage collected=" +
				   key);
		}
	    }
	}
	throw new InvalidSymRefException(key);
    }



    /**
     * Internal SymRef tree structure
     */
    private static class Node {
	public Node parent;
	public Object id;
	public Reference val_ref;

	public Map children;

	public Node() {
	    this(null, null, null);
	}

	public Node(Node parent,
		    Object id,
		    Object value) {
	    this.parent = parent;
	    this.id = id;
	    this.val_ref = new WeakReference(value);
	    children = new HashMap();
	    if (parent != null && parent.children != null) {
		parent.children.put(id, this);
	    }
	}

	/**
	 * Returns the object stored in this node
	 */
	public final SymRefable get() {
	    return (SymRefable)(val_ref.get());
	}

	/**
	 * Insert a value into the root of the tree, possibly overwritting 
	 * existing sub-trees
	 */
	public static List put(Object id, Object val) {
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.
		    logMessage(null,
			       "put id=" + id +
			       " val=" + val);
	    }
	    Node newNode = new Node(root, id, val);
	    List newPath = new LinkedList();
	    newPath.add(id);
	    return newPath;
	}

	/**
	 * Add a value as a child of the given path.
	 * This call will not overwrite any existing children
	 * @return The path to the child
	 */
	public static List addChild(List parentPath, Object id, Object val) {
	    Node parent = root.findNode(parentPath);
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.
		    logMessage(null,
			       "addChild parentPath=" + parentPath +
			       " id=" + id +
			       " val=" + val);
	    }
	    if (parent == null) {
		if (DebuggingSupport.logMessages) {
		    DebuggingSupport.
			logMessage(null,
				   "addChild unable to find parent path=" +
				   parentPath);
		}
		return null;
	    }
	    String newId = id.toString() + "_" + parent.children.size();
	    Node newNode = new Node(parent, newId, val);
	    List newPath = new LinkedList(parentPath);
	    newPath.add(newId);
	    return newPath;
	}
	
	
	/**
	 * @return true iff the given key(path) exists in the tree
	 */
	public static boolean containsKey(List list) {
	    return (root.matchKey(list) == list.size());
	}
	
	/**
	 * returns the number of elements of the path (list) that were
	 * sucessfully matched
	 */
	public static int matchKey(List list) {
	    Node node = root;
	    final int size = list.size();
	    int i = 0 ;
	    for(Iterator it = list.iterator(); it.hasNext(); i++ ) {
		Object id = it.next();
		// check for match in current nodes children
		if (node.children.containsKey(id)) {
		    node = (Node)node.children.get(id);
		} else {
		    break;
		}
	    }
	    return i;
	}
	
	/**
	 * returns the node at the end of this path, if it was
	 * sucessfully matched
	 */
	public static Node findNode(List list) {
	    Node node = root;
	    final int size = list.size();
	    for(Iterator it = list.iterator(); it.hasNext(); ) {
		Object id = it.next();
		// check for match in current nodes children
		if (node.children.containsKey(id)) {
		    node = (Node)node.children.get(id);
		} else {
		    return null;
		}
	    }
	    return node;
	}
	
	
	

    }

}

