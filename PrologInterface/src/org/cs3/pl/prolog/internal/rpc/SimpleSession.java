package org.cs3.pl.prolog.internal.rpc;

import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.internal.ReusablePool;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.MessagingClientLoggingWrapper;

/**
 * SimpleSession is the basic Session. It does not provide any feedback to the
 * user, but provides a small, yet effective way of interacting with the prolog
 * System. This is the default implementation of getSession() in PrologInterface
 * 
 * @author terra
 * @inheritDoc
 */
public class SimpleSession implements PrologSession {

    private ReusablePool connectionPool;

    protected boolean disposed;

    protected boolean queryActive = false;

    private ReusableRPCClient rpcDelegate = null;

    /**
     * constructs a SimpleSession. The Server is connected on the local host, at
     * the passed port.
     * 
     * @param port
     *                    the port to connect to
     * @throws IOException
     *                    a connection failure occurs.
     */

    public SimpleSession(int port) throws IOException {
        rpcDelegate = new ReusableRPCClient();
        rpcDelegate.enableLogging(new Logger("default"));
        rpcDelegate.configure("localhost", port);

        Debug.info("Client configured, connecting to server");
        rpcDelegate.start();

    }

    public SimpleSession(ReusableRPCClient connection) {
        rpcDelegate = connection;
        Debug.info("Session created, reusing existing connection");

    }

    public boolean consult(String name) {
        boolean windowsPlattform = System.getProperty("os.name").indexOf(
                "Windows") > -1;
        if (windowsPlattform)
            name = name.replace('\\', '/');

        try {
            return query("consult('" + name + "')") != null;
        } catch (PrologException e) {
            Debug.report(e);
        }

        return false;
    }

    synchronized public void dispose() {
        Debug.debug("disposed called");
        if (!disposed) {
            queryActive = false;
            disposed = true;
            if (connectionPool != null) {
                connectionPool.recycle(rpcDelegate);
            }
        }
    }

    synchronized protected Hashtable doNext() throws PrologException {
        if (!queryActive)
            throw new PrologException("No query is currently active");

        Hashtable rv;

        try {
            rv = (Hashtable) getRpcDelegate().call("RemotePrologSession",
                    "next", new Object[] {});
        } catch (Exception e) {
            PrologException ex = new PrologException(
                    "Error while executing call");
            ex.initCause(e);
            throw ex;
        }

        if (rv == null)
            queryActive = false;

        return rv;
    }

    synchronized protected Hashtable doQuery(String query)
            throws PrologException {

        if (queryActive) {
            Debug.debug("Implicitly ending query");
            endQuery();
        }

        queryActive = true;

        Hashtable rv;

        try {
            rv = (Hashtable) getRpcDelegate().call("RemotePrologSession",
                    "query", new Object[] { query });
        } catch (Exception e) {
            PrologException ex = new PrologException(
                    "Error while executing call " + query);
            ex.initCause(e);
            throw ex;
        }

        if (rv == null)
            queryActive = false;

        return rv;
    }

    /**
     * @param query
     * @return
     * @throws PrologException
     */
    private Hashtable[] doQueryAll(String query) throws PrologException {
        if (queryActive) {
            Debug.debug("Implicitly ending query");
            endQuery();
        }

        queryActive = true;
        Hashtable[] rv;

        try {
            rv = (Hashtable[]) getRpcDelegate().call("RemotePrologSession",
                    "queryAll", new Object[] { query });
        } catch (Exception e) {
            PrologException ex = new PrologException(
                    "Error while executing call");
            ex.initCause(e);
            throw ex;
        }

        queryActive = false;

        return rv;
    }

    public void endQuery() throws PrologException {
        if (disposed)
            throw new IllegalStateException("Session is disposed");

        if (!queryActive)
            return;

        queryActive = false;
    }

    public void finalize() throws Throwable {
        if (!disposed) {
            Debug.warning("Implicitly disposing Session in finalizer");
            dispose();
        }

    }

    /**
     * @return Returns the connectionPool.
     */
    public ReusablePool getConnectionPool() {
        return connectionPool;
    }

    protected MessagingClientLoggingWrapper getRpcDelegate() {
        return rpcDelegate;
    }

    public boolean isDisposed() {
        return disposed;
    }

    synchronized public Hashtable next() throws PrologException {
        if (disposed)
            throw new IllegalStateException("Session is disposed");

        PrologSynchronizer.getInstance().beginAccess();

        try {
            return doNext();
        } finally {
            PrologSynchronizer.getInstance().endAccess();
        }
    }

    synchronized public Hashtable query(String query) throws PrologException {
        if (disposed) {
            throw new IllegalStateException("Session is disposed");
        }

        PrologSynchronizer.getInstance().beginAccess();

        try {
            return doQuery(query);
        } finally {
            PrologSynchronizer.getInstance().endAccess();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#queryAll(java.lang.String)
     */
    public Hashtable[] queryAll(String query) throws PrologException {
        if (disposed)
            throw new IllegalStateException("Session is disposed");

        PrologSynchronizer.getInstance().beginAccess();

        try {
            return doQueryAll(query);
        } finally {
            PrologSynchronizer.getInstance().endAccess();
        }
    }

    /**
     * @param connectionPool
     *                    The connectionPool to set.
     */
    public void setConnectionPool(ReusablePool connectionPool) {
        this.connectionPool = connectionPool;
    }

    /**
     * @param rpcDelegate
     *                    The rpcDelegate to set.
     */
    public void setRpcDelegate(ReusableRPCClient rpcDelegate) {
        this.rpcDelegate = rpcDelegate;
    }

}
