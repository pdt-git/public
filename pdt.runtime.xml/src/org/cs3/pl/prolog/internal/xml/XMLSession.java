/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.prolog.internal.xml;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.xml.stream.XMLStreamException;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.ReusablePool;

/**
 */
public class XMLSession implements PrologSession {
    PrologInterface pif;
    private XMLClient client;
    private ReusablePool pool;
    private boolean disposed=false;
    private int nextIndex=-1;
    private String nextQuery=null;

    /**
     * 
     */
    public XMLSession(ReusablePool pool, XMLClient client,PrologInterface pif) {
        this.pool=pool;
        this.client=client;
        this.pif=pif;
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#dispose()
     */
    public void dispose() {
        if(disposed){
            throw new IllegalStateException("session is disposed");
        }
        pool.recycle(client);
        disposed=true;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#query(java.lang.String)
     */
    public Map query(String query) throws PrologException {
        if(disposed){
            throw new IllegalStateException("session is disposed");
        }
        // we do no interacteve queries, since they suck.
        //insteaad we fake them.
        nextIndex=1;
        nextQuery=query;
        return queryOnce(query);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#queryOnce(java.lang.String)
     */
    public Map queryOnce(String query) throws PrologException {
        if(disposed){
            throw new IllegalStateException("session is disposed");
        }
        if(! client.ok()){
            throw new PrologException("XMLClient is in no useable state.");
        }
        client.clear();
        try {
            client.writeln("query_once(query)");
            client.dispatch_until_next(XMLClient.OK);
            if(client.getExceptions().size()>0){
                throw new PrologException("Peer reported one or more exceptions: "+Util.prettyPrint(client.getExceptions().toArray()));
            }
            if(client.getFailed().size()>0){
                throw new PrologException("Peer reported one or more failed goals: "+Util.prettyPrint(client.getExceptions().toArray()));
            }
            return Collections.unmodifiableMap(client.getSolution());
        } catch (XMLStreamException e) {
            throw new PrologException("XMLCLient reported a problem",e);
        }
        
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#queryAll(java.lang.String)
     */
    public List queryAll(String query) throws PrologException {
        if(disposed){
            throw new IllegalStateException("session is disposed");
        }
        if(! client.ok()){
            throw new PrologException("XMLClient is in no useable state.");
        }
        client.clear();
        try {
            client.writeln("query_once(query)");
            client.dispatch_until_next(XMLClient.OK);
            if(client.getExceptions().size()>0){
                throw new PrologException("Peer reported one or more exceptions: "+Util.prettyPrint(client.getExceptions().toArray()));
            }
            if(client.getFailed().size()>0){
                throw new PrologException("Peer reported one or more failed goals: "+Util.prettyPrint(client.getExceptions().toArray()));
            }
            return client.getSolutions();
        } catch (XMLStreamException e) {
            throw new PrologException("XMLCLient reported a problem",e);
        }
        
        
    }
    

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#next()
     */
    public Map next() throws PrologException {
        if(disposed){
            throw new IllegalStateException("session is disposed");
        }
        if(nextIndex==1){
            queryAll(nextQuery);
        }
        Vector s = client.getSolutions();
        if(s.size()>nextIndex){
            Map r = (Map) s.get(nextIndex);
            nextIndex++;
            return r;
        }
//        else {
            return null;
  //      }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#endQuery()
     */
    public void endQuery() throws PrologException {
        // uahh...<gähn>
        ;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String)
     */
    public boolean consult(String name) {
        boolean windowsPlattform = System.getProperty("os.name").indexOf(
                "Windows") > -1;
        if (windowsPlattform){           
            name = name.replace('\\', '/');
        }
        return query("consult('" + name + "')") != null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#isDisposed()
     */
    public boolean isDisposed() {
        return disposed;
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String, java.io.InputStream)
     */
    public void consult(String name, InputStream content) throws PrologException {
        if(disposed){
            throw new IllegalStateException("session is disposed");
        }
        if(! client.ok()){
            throw new PrologException("XMLClient is in no useable state.");
        }
        client.clear();
        try {
            client.writeln("consult(name)");
            OutputStream out = client.getOut();
            Util.copy(content,out);
            
            client.dispatch_until_next(XMLClient.OK);
            if(client.getExceptions().size()>0){
                throw new PrologException("Peer reported one or more exceptions: "+Util.prettyPrint(client.getExceptions().toArray()));
            }
            if(client.getFailed().size()>0){
                throw new PrologException("Peer reported one or more failed goals: "+Util.prettyPrint(client.getExceptions().toArray()));
            }            
        } catch (XMLStreamException e) {
            throw new PrologException("XMLCLient reported a problem",e);
        } catch (IOException e) {
            throw new PrologException("IO Problem during consult.",e);
        }
        
        
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#unconsult(java.lang.String)
     */
    public void unconsult(String name) throws PrologException {
        // TODO Auto-generated method stub
        
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#isConsulted(java.lang.String)
     */
    public boolean isConsulted(String name) throws PrologException {
        // TODO Auto-generated method stub
        return false;
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#getPrologInterface()
     */
    public PrologInterface getPrologInterface() {
       return pif;
    }

}
