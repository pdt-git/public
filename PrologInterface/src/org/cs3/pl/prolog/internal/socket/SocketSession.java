/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.IOException;
import java.util.Hashtable;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class SocketSession implements PrologSession {

    private SocketClient client;

    private boolean queryActive;

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
    public Hashtable query(String query) throws PrologException {
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
                client.writeln(query);
            } else {
                client.writeln(query + ".");
            }
            queryActive = true;
            solution = read_solution();

        } catch (IOException e) {
            client.unlock();
            throw new PrologException(e);
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
    public Hashtable[] queryAll(String query) throws PrologException {
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
                client.writeln(query);
            } else {
                client.writeln(query + ".");
            }
            Vector results = new Vector();
            Hashtable result = read_solution();
            while (result != null) {
                results.add(result);
                result = read_solution();

            }
            return (Hashtable[]) results.toArray(new Hashtable[0]);
        } catch (IOException e) {
            throw new PrologException(e);
        } finally {
            client.unlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#next()
     */
    public Hashtable next() throws PrologException {
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
            throw new PrologException(e);
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
                            + line.substring(SocketClient.ERROR.length()));
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
                int eqPos = line.indexOf('=');
                //Debug.debug("eqPos="+eqPos);
                String name = line.substring(1, eqPos - 1);
                String value = line.substring(eqPos + 1);
                result.put(name, value);
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

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#isDisposed()
     */
    public boolean isDisposed() {
        // TODO Auto-generated method stub
        return client == null;
    }

}
