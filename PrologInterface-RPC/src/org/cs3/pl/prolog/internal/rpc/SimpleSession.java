package org.cs3.pl.prolog.internal.rpc;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
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
            Map r = query("consult('" + name + "')");
            if(r!=null&&dispatcher!=null){
                dispatcher.update(new PrologInterfaceEvent(this,PrologInterface.SUBJECT_CONSULTED,name));
            }
            return r != null;
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

    synchronized public Map next() throws PrologException {
        if (disposed)
            throw new IllegalStateException("Session is disposed");

        PrologSynchronizer.getInstance().beginAccess();

        try {
            return doNext();
        } finally {
            PrologSynchronizer.getInstance().endAccess();
        }
    }

    synchronized public Map query(String query) throws PrologException {
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
    public List queryAll(String query) throws PrologException {
        if (disposed)
            throw new IllegalStateException("Session is disposed");

        PrologSynchronizer.getInstance().beginAccess();

        try {
            Hashtable[] r = doQueryAll(query);
            ArrayList l = new ArrayList(r.length);
            for (int i = 0; i < r.length; i++) {
                l.add(r[i]);
            }
            return l;
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

    /*
     * (non-Javadoc)
     * 
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
     * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String,
     *           java.io.InputStream)
     */
    public void consult(String name, InputStream content)
            throws PrologException {
        try {
            File f = new File(System.getProperty("java.io.tmpdir", name));
            f.createNewFile();
            BufferedOutputStream out = new BufferedOutputStream(
                    new FileOutputStream(f));
            Util.copy(content, out);            
            out.close();
            consult(f.getCanonicalPath());
        } catch (IOException e) {
            throw new PrologException(e);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#unconsult(java.lang.String)
     */
    public void unconsult(String name) {
        try {
            File f = new File(System.getProperty("java.io.tmpdir", name));
            if (f.exists()) {
                f.delete();
            }
            f.createNewFile();
            consult(f.getCanonicalPath());
        } catch (IOException e) {
            throw new PrologException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologSession#isConsulted(java.lang.String)
     */
    public boolean isConsulted(String name) {
        boolean windowsPlattform = System.getProperty("os.name").indexOf(
                "Windows") > -1;
        if (windowsPlattform)
            name = name.replace('\\', '/');

        return queryOnce("source_file('" + name + "')") != null;
    }

}
