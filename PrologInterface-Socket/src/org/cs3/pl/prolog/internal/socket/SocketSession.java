/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class SocketSession implements PrologSession {

    private SocketClient client;

    private boolean queryActive;

    private String lastQuery;

    public SocketSession(SocketClient client) {
        this.client = client;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#dispose()
     */
    public void dispose() {
        if(isDisposed()){
            return;
        }
        try {
            client.lock();
            client.close();
        } catch (IOException e) {
            Debug.report(e);
            throw new RuntimeException(e);
        } finally {
            client.unlock();
            client = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#query(java.lang.String)
     */
    public Map query(String query) throws PrologException {
        if(isDisposed()){
            throw new IllegalStateException("Session is disposed!");
        }
        Hashtable solution;
        endQuery();
        client.lock();
        try {
            client.readUntil(SocketClient.GIVE_COMMAND);
            client.writeln(SocketClient.QUERY);
            client.readUntil(SocketClient.GIVE_TERM);
            query = query.trim();
            
            if (query.endsWith(".")) {
                this.lastQuery=query;
                client.writeln(query);
            } else {
                this.lastQuery=query+".";
                client.writeln(query + ".");
            }
            
            queryActive = true;
            solution = read_solution();

        } catch (IOException e) {
            client.unlock();
            throw new PrologException("got io trouble. last query was: "+lastQuery,e);
        }
        if (solution == null) {
            endQuery();
        }
        return solution;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#queryAll(java.lang.String)
     */
    public List queryAll(String query) throws PrologException {
        if(isDisposed()){
            throw new IllegalStateException("Session is disposed!");
        }
        endQuery();
        client.lock();
        try {
            client.readUntil(SocketClient.GIVE_COMMAND);
            client.writeln(SocketClient.QUERY_ALL);
            client.readUntil(SocketClient.GIVE_TERM);
            query = query.trim();
            if (query.endsWith(".")) {
                this.lastQuery=query;
                client.writeln(query);
            } else {
                this.lastQuery=query+".";
                client.writeln(query + ".");
            }
            Vector results = new Vector();
            Hashtable result = read_solution();
            while (result != null) {
                results.add(result);
                result = read_solution();

            }
            return results;
        } catch (IOException e) {
            throw new PrologException("got io problems. last query was: "+lastQuery,e);
        } finally {
            client.unlock();
        }
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#queryOnce(java.lang.String)
     */
    public Map queryOnce(String query) throws PrologException {
        Map result = query(query);
        endQuery();
        return result;
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#next()
     */
    public Map next() throws PrologException {
        if(isDisposed()){
            throw new IllegalStateException("Session is disposed!");
        }
        if(!queryActive){
            throw new IllegalStateException("No query active.");
        }
        client.lock();
        Hashtable solution = null;
        try {
            client.readUntil(SocketClient.MORE);
            client.writeln(SocketClient.YES);
            solution = read_solution();
            if (solution == null) {
                endQuery();
            }
        } catch (IOException e) {
            throw new PrologException("got io problems. last query was: "+lastQuery,e);
        } finally {
            client.unlock();
        }
        return solution;
    }

    /**
     * @return
     * @throws IOException
     * @throws PrologException
     */
    private Hashtable read_solution() throws PrologException, IOException {
        client.lock();
        Hashtable result = new Hashtable();
        try {
            while (true) {
                
                String line = client.readln();
                //Debug.debug("parsing: "+line);
                if (line == null) {
                    throw new PrologException("don't know what to do.");
                }
                if (line.startsWith(SocketClient.ERROR)) {
                    throw new PrologException("Peer reported an error:"
                            + line.substring(SocketClient.ERROR.length())+"\n" +
                            		"Last query was: "+lastQuery);
                }
                if (SocketClient.END_OF_SOLUTION.equals(line)) {//yes
                    return result;
                }
                if (SocketClient.NO.equals(line)) {//no
                                                                      // further
                                                                      // solutions
                    return null;
                }
                if (SocketClient.OK.equals(line)) {//no
                    // further
                    // solutions
                    return null;
                }
                if(!(line.charAt(0)=='<')){
                    throw new RuntimeException("expected '<' at begin of line");
                }
                StringBuffer buf = new StringBuffer(line);
                //make sure we read the complete binding
                Debug.debug("first line: "+line);
                while(!line.endsWith(">")){
                    line = client.readln();
                    buf.append("\n"+line);
                    Debug.debug("appended: "+line);
                }
                line = buf.toString();

                //read the variable name   
                int start=1;
                int end = line.indexOf('>');
                if(end<start){
                    throw new RuntimeException("ill-formated solution line: "+line);
                }
                String name = Util.unescape(line,start,end);
                
                //read the variable value
                start=line.indexOf('<',end)+1;
                if (start<end){
                    throw new RuntimeException("ill-formated solution line: "+line);
                }
                end=line.indexOf('>',start);
                if(end<start){
                    throw new RuntimeException("ill-formated solution line: "+line);
                }
                String value = Util.unescape(line,start,end);
                result.put(name, value);
//                
//                int eqPos = line.indexOf('=');
//                //Debug.debug("eqPos="+eqPos);
//                String name = line.substring(1, eqPos - 1);
//                String value = line.substring(eqPos + 1).trim();
//                if(value.startsWith("'")&&value.endsWith("'")){
//                    value=value.substring(1,value.length()-1);
//                    
//                }
//                result.put(name, value);
            }
        } finally {
            client.unlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#endQuery()
     */
    public void endQuery() throws PrologException {
       if(isDisposed()){
           throw new IllegalStateException("Session is disposed!");
       }
       if(!queryActive){
           return;
       }
        client.lock();
        try {
            while (true) {
                String line = client.readln();
                if (line == null) {
                    throw new PrologException("don't know what to do.");
                }
                if (SocketClient.MORE.equals(line)) {
                    client.writeln(SocketClient.NO);
                }
                if (SocketClient.OK.equals(line)) {
                    return;
                }
                if (line.startsWith(SocketClient.ERROR)) {
                    throw new PrologException("Peer reported an error:"
                            + line.substring(SocketClient.ERROR.length()));
                }
            }
        } catch (IOException e) {
            throw new PrologException(e);
        } finally {
            //this is no typo!
            //we need to release lock TWO times:
            //inner lock: in this method.
            //outer lock: in preceeding call to query.
            queryActive = false;
            client.unlock();
            client.unlock();

        }

    }
    private PrologInterfaceListener dispatcher=null;
    /**
     * @return Returns the dispatcher.
     */
    public PrologInterfaceListener getDispatcher() {
        return dispatcher;
    }
    /**
     * @param dispatcher The dispatcher to set.
     */
    public void setDispatcher(PrologInterfaceListener dispatcher) {
        this.dispatcher = dispatcher;
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
        Map r = query("consult('" + name + "')");
        if(r!=null&&dispatcher!=null){
            dispatcher.update(new PrologInterfaceEvent(this,PrologInterface.SUBJECT_CONSULTED,name));
        }
        return r!=null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#isDisposed()
     */
    public boolean isDisposed() {
        // TODO Auto-generated method stub
        return client == null;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String, java.io.InputStream)
     */
    public void consult(String name, InputStream content) throws PrologException {
        try {
            OutputStream out = new ConsultOutputStream(client,name);
            Util.copy(content,out);
            out.close();
            if(dispatcher!=null){
                dispatcher.update(new PrologInterfaceEvent(this,PrologInterface.SUBJECT_CONSULTED,name));
            }
        } catch (IOException e) {
            throw new PrologException(e);
        }
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#unconsult(java.lang.String)
     */
    public void unconsult(String name) throws PrologException {
        try {
            OutputStream out = new ConsultOutputStream(client,name);
            out.close();
            if(dispatcher!=null){
                dispatcher.update(new PrologInterfaceEvent(this,PrologInterface.SUBJECT_CONSULTED,name));
            }
        } catch (IOException e) {
            throw new PrologException(e);
        }
      
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologSession#isConsulted(java.lang.String)
     */
    public boolean isConsulted(String name) throws PrologException {
        return queryOnce("source_file('" + name + "')") != null;
    }

}
