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
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.ReusablePool;

/**
 */
public class XMLSession implements PrologSession {

    private XMLClient client;
    private ReusablePool pool;
    private boolean disposed=false;
    private int nextIndex=-1;
    private String nextQuery=null;

    /**
     * 
     */
    public XMLSession(ReusablePool pool, XMLClient client) {
        this.pool=pool;
        this.client=client;
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

}
