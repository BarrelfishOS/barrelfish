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
// Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): Stefano Novello / Josh Singer, Parc Technologies
// 
// END LICENSE BLOCK

//Title:        Java/ECLiPSe interface
//Version:      $Id: EXDRInputStream.java,v 1.1 2006/09/23 01:54:10 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  InputStream which can parse ECLiPSe EXDR format.

package com.parctechnologies.eclipse;
import java.util.*;
import java.io.*;



/**
 * A stream which can read EXDR format.
 *
 * An <i>EXDRInputStream</i> can be constructed from any
 * instance of the <i>InputStream</i> class and extends it to be able to interpret
 * incoming data which is in the EXDR (ECLiPSe eXternal Data Representation) format.
 * <p>
 * Use the method {@link EXDRInputStream#readTerm()} to convert from EXDR into
 * Java <i>CompoundTerm</i> objects and instances of other relevant Java classes
 * which represent ECLiPSe types.
 * <p> Note that EXDRInputStream objects are often constructed using the
 * {@link FromEclipseQueue} class.
 * @see CompoundTerm
 * @see FromEclipseQueue
 *
 */
public class EXDRInputStream extends DataInputStream
{
    // note that throughout this class we use the methods of DataInputStream
    // rather than those of the input stream in. This is because we want a
    // blocking read (which blocks until it finishes, or raises an exception)
    // The consequence of this is that this class' methods fulfill the same
    // contract: they block until they are done, or they throw an exception.
    // This is as it should be because this is a subclass of DataInputStream.

    /**
     * The following array list is used to map EXDR string references to their
     * string value. For strings that occur repeatedly throughout the message, 
     * there is a single EXDR string representation. The remainder of the 
     * occurrences are all references to the first occurrence of the string.
     * The array list is not synchronized. Multiple threads writing concurrently
     * to the output stream is envisaged via the synchronized 'readTerm()'. 
     * Should this change, the array should become a Vector or wrapped: 
     * List list = Collections.synchronizedList(new ArrayList(...));
     * An array list is used because string reference idenitifers start from 0,
     * as a result there is not need for a map.
     * Duplicate strings are allowed (EXDR version 2 permits reading but *not*
     * writing of duplicates).
     */ 
    private ArrayList stringList = null;

    /**
     * Construct an <i>EXDRInputStream</i> using a given InputStream for
     * incoming data.
     */
    public EXDRInputStream(InputStream in)
    {
        super(in);
    }

    /**
     * Read a chunk (one term's worth) of EXDR from the incoming data and
     * convert it into the corresponding object (an instance of CompoundTerm, Integer,
     * etc.).
     * This works as an atomic
     * action so no other thread can read until the complete
     * term has been read in.
     */
    synchronized public Object readTerm() throws IOException
    {
        // Read the first two bytes. These give the EXDR version number

        int c1 = readByte();
        int c2 = readByte();
        if (c1 == 'V' && c2 < 3)
        {
            // Dispose of the hash map if it's hanging around 
            // (exception occurred etc)
            stringList = null;
            Object term = readSubTerm();
            // Dispose of the hash map if created
            stringList = null;

            return term;
        }
        else // Throw an exception if the version bytes were incorrect
        {
          throw new IOException("EXDR protocol error: bad version " + c1 + " " + c2);
        }
    }

    /**
     * Read a term's worth of data and return a Java object of the corresponding
     * type.
     */
    private Object readSubTerm() throws IOException
    {
        // Read the first byte, which is a capital letter that varies according
        // to the class of data.
        int c = readByte();
        switch (c)
        {
        case 'C' : // Turn string compression on
            stringList = new ArrayList();
            return readSubTerm();
        case 'B' : // for byte use method inherited from DataInputStream
            return new Integer(readByte()); // Implicit widen
        case 'I' : // for integers use method inherited from DataInputStream
            return new Integer(readInt());
        case 'J' : // for long use method inherited from DataInputStream
            return new Long(readLong());
        case 'D' : // for double use method inherited from DataInputStream
            return new Double(readDouble());
        case '_' : // for variables, return null (corresponding java representation)
            return null;
        case 'R' : // Retrieve the string from the string list
            if (stringList == null) {
                throw new IOException(
                    "EXDR protocol error: String compression disabled, but reference found");
            }
            return stringList.get(readNat());
        case 'S' : // use own method for strings
            return readString();
        case 'F' : // for compound terms
            // read the arity as an integer
          int arity = readNat();
            // if arity is zero
            if(arity == 0)
            {
              // read the next term (it will be a string), and
              // construct and return an atom based on it.
              return(new Atom((String) readSubTerm()));
            }
            // construct an array with space for the functor and subterms
            Object res[] = new Object[arity+1];
            // read in the functor and subterms and assign the array
            // elements to the corresponding resulting Objects
            for (int i = 0 ; i < arity+1 ; i++)
                res[i] = readSubTerm();
            // Return a CompoundTermImpl constructed from this array
            return new CompoundTermImpl(res);
        case '[' : // for NON-empty lists
            // create an empty list
            List list = new LinkedList();
            // read and add the first element (there must be one)
            list.add(readSubTerm());
            while(true) // keep reading and adding elements until we reach a ']'
            {
                c = readByte();
                if (c == ']') return list;
                else if (c == '[') list.add(readSubTerm());
                else
                    throw new IOException("EXDR protocol error: unexpected list separator "+c);
            }
        case ']' : // for empty lists
            return Collections.EMPTY_LIST;
        default: // if no codes match, throw an exception
            throw new IOException("EXDR protocol error: unrecognized EXDR code = "+c);
        }
    }

    /**
     * Read an 'XDR_nat'
     */
    private int readNat() throws IOException
    {
        // Read the first byte
        byte a = readByte();

        // If signed bit is one, the remaining 7 bits
        // represent the value.
        if ((a & 0x80) != 0) {
          return (a & 0x7F);
        }

        // If signed bit is 0 it's really a
        // four byte int, so read and construct the value
       
        return (((a & 0xff) << 24) | ((readByte() & 0xff) << 16) |
                ((readByte() & 0xff) << 8) | (readByte() & 0xff));
    }

    /**
     * Read a string
     */
    private String readString() throws IOException
    {
        // create a byte buffer with length for the string
        byte buff[] = new byte[readNat()];

        // read in buff.length bytes into buff
        readFully(buff);

        // construct an intern'd String representation
        // of 'buff'. By interning we save space for large terms 
        // with repeated strings and functors
        String string = (new String(buff)).intern();
        
        // Store the string in the array list for subsequent
        // EXDR string reference resolution
        if (stringList != null) stringList.add(string);

        return string;
    }
}
