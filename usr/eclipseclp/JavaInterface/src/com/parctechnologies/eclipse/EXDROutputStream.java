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
//Version:      $Id: EXDROutputStream.java,v 1.1 2006/09/23 01:54:10 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Output stream which can write EXDR format data.
package com.parctechnologies.eclipse;
import java.util.*;
import java.io.*;
import java.math.BigInteger;

/**
 * A stream which can write EXDR format.
 *
 * An <i>EXDROutputStream</i> can be constructed from any
 * instance of the <i>OutputStream</i> class and extends it to be able to write
 * outgoing data in the EXDR (ECLiPSe eXternal Data Representation) format.
 * <p>
 * Use the method {@link EXDROutputStream#write(Object)} to convert
 * Java <i>CompoundTerm</i> objects and instances of other relevant Java classes into EXDR format
 * so that data can be read in by ECLiPSe.
 * <p>
 * EXDROutputStream also uses a buffer: rather than writing to the underlying OutputStream
 * byte-by-byte, EXDR data is written to the buffer and then copied to the OutputStream
 * when a whole term has been completed.
 * <p> Note that EXDROutputStream objects are often constructed using ToEclipseQueues.
 * @see CompoundTerm
 * @see ToEclipseQueue
 *
 */
public class EXDROutputStream extends FilterOutputStream {

    /**
     * A data output stream is created to do most of the work since EXDR
     * partially compatible with java's own format.
     */
    DataOutputStream out;

    /**
     * This ByteArrayOutputStream is used as a buffer so that data is not
     * written to the underlying output stream byte by byte (this is inefficient).
     * Instead it goes to the ByteArrayOutputStream, whose byte array grows as
     * necessary. The byte array is copied onto the underlying output stream
     * when a full term has been written, or when this stream is flushed.
     */
    ByteArrayOutputStream buf;

    /**
     * The following hash map is used to map EXDR strings to their
     * EXDR string reference identifier. For strings that occur repeatedly
     * throughout the message, there is a single EXDR string representation.
     * The remainder of the occurrences are all references to the first
     * occurrence of the string.
     * The hash map is not synchronized. Multiple threads writing concurrently
     * to the output stream is envisaged via the synchronized 'write()'. 
     * Should this change, the map should become a Hashtable or wrapped: 
     * Map m = Collections.synchronizedMap(new HashMap(...));
     */ 
    private HashMap stringReferenceMap = null;

    private boolean compressStrings = false;

    /**
     * Construct an <i>EXDROutputStream</i> which will write
     * EXDR to a named OutputStream.
     */
    public EXDROutputStream(OutputStream s)
    {
        
        // execute superclass's constructor, setting the underlying output
        // stream to s. Later this is referenced as super.out.
        super(s);
        // create a byte array as an output stream
        buf = new ByteArrayOutputStream();
        // decorate byte array with a DataOutputStream (so we can write integers
        // to it etc). This is where bytes will be written.
        out = new DataOutputStream(buf);
    }

    /**
     * Construct an <i>EXDROutputStream</i> which will write
     * EXDR with optionally compressed strings to a named OutputStream.
     */
    public EXDROutputStream(OutputStream s, boolean compressStrings)
    {
        this(s);
	this.compressStrings = compressStrings;
    }

    /**
     * Write the version to the buffer as per EXDR format
     */
    private void writeVersion() throws IOException
    {
      out.writeByte('V');
      out.writeByte(2);
    }

    /**
     * Write an Object to the underlying stream in EXDR format. The Object (or if it is
     * a compound term, its arguments, nested
     * however deep) can be of any of the following acceptable EXDR output types:
     * <ul>
     * <li><code>null</code> - this is interpreted as a variable in ECLiPSe. </li>
     * <li><i>String</i> objects.</li>
     * <li>Anything implementing the <i>CompoundTerm</i> interface whose arguments are
     * also instances of acceptable EXDR output types. </li>
     * <li><i>Integer</i> objects.<li>
     * <li><i>Double</i> or <i>Float</i> objects: these are interpreted as floats
     * on the ECLiPSe side. Note that attempts to write Not-a-number (NaN) will raise an IllegalArgumentException.<li>
     * <li>Any object implementing the <i>Collection</i> interface whose elements
     * are all instances of acceptable EXDR output types. The collection is interpreted
     * as an ECLiPSe list.</li>
     * </ul>
     * This is an atomic action, so that different threads
     * writing to the same stream do not garble the data.
     * @throws IllegalArgumentException if the parameter is of an unrecognised
     * class, or if Not-a-number (NaN)-valued Floats or Doubles are
     * supplied.
     */
    public synchronized void write(Object o) throws IOException
    {
       // write the version to the buffer
       writeVersion();
       // if compressing streams
       if (compressStrings) {
           // Write compression header byte
           out.writeByte('C');
           // Create the hash map used for EXDR string reference lookups
           stringReferenceMap = new HashMap();
       } else {
           // Make sure the hash map is NULL just in case of previous exception etc 
           stringReferenceMap = null;
       } 
       // write the term o to the buffer
       writeSub(o);
       // put all the bytes accumulated in the buffer on to the underlying
       // output stream
       //System.err.println("EXDROutStream "+this+" buf size="+buf.size());
       flush_buffer();
       // Dispose of the hash map (if created)
       stringReferenceMap = null;
    }

  /**
   * puts all the bytes accumulated in the buffer on to the underlying
   * output stream.
   */
    private void flush_buffer() throws IOException
    {
      // convert the ByteArrayOutputStream to a byte array and write these
      // bytes to the underlying OuputStream stored as an instance variable
      // in the superclass
      super.out.write(buf.toByteArray());
      // reset (i.e. empty) the buffer
      buf.reset();
    }

    /**
     *  Write a term o's worth of bytes to the buffer.
     */
    void writeSub(Object o) throws IOException
    {
        if (o == null) // null means a variable
            writeVar();
        else if (o instanceof Collection) // any collection is a list
            writeList((Collection) o);
        else if (o instanceof CompoundTerm) // compound term
            writeStructure((CompoundTerm) o);
        else if (o instanceof Integer) // integer
            writeInt(((Integer)o).intValue());
        else if (o instanceof Long) // long
            writeLong(((Long)o).longValue());
        else if (o instanceof Double) // double
            writeDouble(((Double)o).doubleValue());
        else if (o instanceof Float) // float
            // For floats, we convert a double, then write it
            writeDouble(((Float) o).doubleValue());
        else if (o instanceof String) // string
            writeString(((String) o));
        else // throw an IllegalArgument exception if the object is not of a
             // recognised class
            throw(new IllegalArgumentException("Attempt to write EXDR for non-"+
                  "recognised class "+o.getClass().getName()));
    }

    /**
     * Write a double to the buffer in EXDR format.
     *
     * @throws IllegalArgumentException if NaN (not-a-number) is passed in as
     * the parameter.
     */
    void writeDouble(double d) throws IOException
    {
        if(Double.isNaN(d))
        {
          throw(new IllegalArgumentException("NaN cannot be written"+
           " in EXDR format."));
        }
        out.writeByte('D');
        out.writeDouble(d);
    }

    /**
     * Write a string to the buffer in EXDR format
     */
    void writeString(String s) throws IOException
    {
        int val;
        boolean wroteReference = false;

        /**
         * In EXDR version 2, we have added a form of string
         * compression to reduce the message sizes. If a string
         * has previously been written to the stream then we 
         * just write out a reference to it. In addition,
         * if the length of the string < 128 we write an XDR_nat
         * not an XDR_int;
         */

        if (stringReferenceMap != null) {
            /**
             * We don't allow mappings of keys to a 'null' value. 
             * The return of 'null' really does indicate that there
             * is not a key-value pair in the map.
             */
            Integer strRefID = (Integer)stringReferenceMap.get(s);
      
            if ( strRefID == null ) {
                // No previous occurrences, write as a EXDR string
                stringReferenceMap.put(s, new Integer(stringReferenceMap.size()));
                out.writeByte('S');
                val = s.length();
            } else { 
                // Previous occurrences, write as a EXDR string reference
                out.writeByte('R');
                val = strRefID.intValue();
                wroteReference = true;
            }
        } else {
            out.writeByte('S');
            val = s.length();
        }
      
        // Can we write the Length/Index as a XDR_nat?
        if (val == (int)(byte)val) {
            out.writeByte(val | 0x80);
        } else {
            out.writeInt(val);
        }
      
        // If writing an EXDR string, write the characters
        if (!wroteReference) {
            out.writeBytes(s);
        }
    }

    /**
     * Write an integer to the buffer in EXDR format
     */
    void writeInt(int i) throws IOException
    {
        if ( i == (int) (byte)i ) 
        {
            out.writeByte('B');
            out.writeByte((byte)i);
        
        } else {
            out.writeByte('I');
            out.writeInt(i);
        }
    }

    /**
     * Write a long to the buffer in EXDR format. However, if the number
     * is small enough to be represented as a regular int, an int is
     * written instead.
     */
    void writeLong(long l) throws IOException
    {
        if (l == (long) (int) l)
                writeInt((int) l);
        else
        {
            out.writeByte('J');
            out.writeLong(l);
        }
    }

    /**
     * Write a variable to the buffer in EXDR format
     */
    void writeVar() throws IOException
    {
        out.writeByte('_');
    }

    /**
     * Write a compound term to the buffer in EXDR format
     */
    void writeStructure(CompoundTerm term) throws IOException
    {
        out.writeByte('F');
        // arity
        out.writeInt(term.arity());
        // functor
        writeString(term.functor());
        // and arguments
        for( int i=1 ; i <= term.arity() ; i++)
            writeSub(term.arg(i));
    }

    /**
     * Write a list to the buffer in EXDR format. Order of list is as per
     * iterator.
     */
    void writeList(Collection collection) throws IOException
    {
        // get an iterator
        Iterator i = collection.iterator();
        while (i.hasNext())
        {
            // write each element
            out.writeByte('[');
            writeSub(i.next());
        }
        out.writeByte(']');
    }

    /**
     * Flushes the underlying <i>OutputStream</i>.
     */
    public void flush() throws IOException
    {
      // make sure any buffered data is already flushed on to super.out.
      flush_buffer();
      super.out.flush();
    }

    /**
     * Enable / disable string compression for subsequently written terms.
     */
    public void enableCompression(boolean compress)
    {
	this.compressStrings = compress;
    }
}
