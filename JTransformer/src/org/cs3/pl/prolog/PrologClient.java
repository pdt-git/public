/*
 * Created on 21.01.2004
 * 
 * To change the template for this generated file go to Window - Preferences -
 * Java - Code Generation - Code and Comments
 */
package org.cs3.pl.prolog;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Properties;
import java.util.Vector;

import jpl.JPLException;
import jpl.PrologException;

import org.cs3.pl.Debug;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.MessagingClientLoggingWrapper;
import org.rapla.components.rpc.ServiceNotFoundException;
import org.rapla.components.rpc.TimeoutException;

/**
 */
public class PrologClient extends MessagingClientLoggingWrapper implements
 IPrologClient{
    private Properties properties;

    public static final String METADATA = "meta_data";

    public static final String METADATAHELP = METADATA + "_help";

    public static final String METADATAMODULE = METADATA + "_module";

    private static boolean WINDOWSPLATTFORM;

    public static String[] PROLOGEXECUTABLESWIN = { "plcon", "plwin" };

    public static String[] PROLOGEXECUTABLES = { "swipl", "xpce" };

    public static String xpce_win = "consult(swi('plwin.rc')).";

    public static String XPCEPREFIX = "konsole -e "; //--name

    // \"SWI
    // PROLOG\"

    public String DEFAULTOUTDIR = "out_laj";

    public final static String GENCLASSFILENAMES = "gen_classfile_names.txt";

    private boolean halt = true;

    private boolean guitracer = false;

    private Vector spyPoints = new Vector();

    private boolean firstRule = true;

    private boolean silent = false;

    private boolean processing = false;

    public static String ROLE = IPrologServer.class.getName();//FIXME: ld: this is correc, i presume?

    String java2prologDir = "";

    String classpath = " ";

    int ctransformerr = 0;

    private boolean showConsole = false;

    private PrologManager manager;

    private boolean hidden;

    PrologClient(PrologManager manager, boolean hidden) {
        this.manager = manager;
        this.hidden = hidden;
    }

    static private Hashtable predicates = new Hashtable();

    private boolean queryActive = false;

    private boolean initdone = false;

    private boolean inCall = false;

	private Object queryActiveLock = new Object();

    private Object initLock = new Object();

    /**
     * @param queryActive
     *                    The queryActive to set.
     */
    void setQueryActive(boolean queryActive) {
    	synchronized(queryActiveLock) {
    		this.queryActive = queryActive;
    	}
    }

    private boolean shutdown = false;

    private static Object shutdownMonitor = new Object();

    
    private Object callLock = new Object();

    private HashSet prologListeners= new HashSet();;

//    public void setPredicates(String filename, PrologCompiler checker) {
//        predicates.put(makeFilenameSWIConform(filename), checker
//                .getPrologElements());
//    }

    /**
     * Initializes the main Prolog Client. At first it starts a new Prolog
     * Server and binds it to port PORT. Then it connects the current client to
     * this server.
     */

    //FIXME: this should be private or protected!
    void reconnect() throws IOException {
        synchronized (shutdownMonitor) {
            shutdown = false;
        }
        connectToServer();
    }

    public void connectToServer() throws IOException {
        if (isRunning())
            throwIllegalServerStateException();
        //int port = PORT;
        configure("localhost", port);
        enableLogging(new Logger("client"));
        getLogger().setDebug(false);
        start();
        synchronized (initLock) {
            initdone = true;
            initLock.notifyAll();
        }
    }

    /**
     * @param string
     */
    private boolean consultLibrary(String module) {
        return ((Boolean) catchedCall(ROLE, "consultLibrary",
                new Object[] { module })).booleanValue();
    }

    synchronized private Object catchedCall(String role, String methodName,
            Object[] args) {
        synchronized (shutdownMonitor) {
            if (shutdown)
                throw new IllegalStateException("Server is shutting down.");
        }
        synchronized (callLock) {
            inCall = true;
        }
        try {
            if (!isRunning())
                start();
            fireEnterCatchedCall(new PrologEvent(this));
            Object ret = call(role, methodName, args);
            return ret;
        } catch (TimeoutException e) {
            Debug.report(e);
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof AbortException
                    || e.getCause() instanceof PrologException
            /*
             * && ((PrologException) e.getCause()).term().toString()
             * .equals("$aborted")
             */) {
                //manager.fireNewDataAvailablel(e.getCause().getMessage(),PrologManager.OUT_STREAM);
                abort();
                if(e.getCause() instanceof PrologException && isHidden()) {
                	PrologException pe = (PrologException)e.getCause();
                	throw new RuntimeException(e.getCause());
                }

            } else if (e.getCause() instanceof JPLException) {
                Debug.report(e.getCause());
                //manager.updatePrologListeners(PrologManager.ERR_STREAM, e.getCause().getMessage());
                abort();
            } else
                Debug.report(e);
        } catch (ServiceNotFoundException e) {
            Debug.report(e);
        } catch (IOException e) {
            Debug.report(e);
        } finally {
            synchronized(callLock){
                inCall = false;
                fireExitCatchedCall(new PrologEvent(this));
            }
        }

        trace("cc fails: " + methodName);
        return null;
    }

//    private void fireEnterCatchedCall() {
//        // TODO Auto-generated method stub
//        
//    }
//
//    private void fireExitCatchedCall() {
//        // TODO Auto-generated method stub
//        
//    }

    void fireEnterCatchedCall(PrologEvent e) {
		//ld:hier nicht erforderlich! siehe addPrologListener
	    HashSet cloned = null;
		synchronized (prologListeners) {
		    cloned = (HashSet) prologListeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
            IPrologListener l = (IPrologListener) it.next();
            l.enterCatchedCall(e);
        }
	}
    
    void fireExitCatchedCall(PrologEvent e) {
		//ld:hier nicht erforderlich! siehe addPrologListener
	    HashSet cloned = null;
		synchronized (prologListeners) {
		    cloned = (HashSet) prologListeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
            IPrologListener l = (IPrologListener) it.next();
            l.exitCatchedCall(e);
        }
	}
    //	public void runQuery(String query, Term[] term) {
    //		setProperties(null);
    //		manager.updatePrologListeners(PrologManager.IN_STREAM, /* "?- " + */query
    // + "\n");
    //		try {
    //			manager.getPrologWriter().write(query);
    //			manager.getPrologWriter().newLine();
    //			manager.getPrologWriter().flush();
    //		} catch (IOException e) {
    //			Debug.report(e);
    //		}
    //	}
   

    /**
     * @param string
     * @return
     */
    private String removeQuotes(String str) {
        if (str == null || str.length() < 2)
            return str;
        if (str.charAt(0) == '\'' && str.charAt(str.length() - 1) == '\'')
            return str.substring(1, str.length() - 1);
        return str;
    }

    public void setProperties(Properties properties) {
        this.properties = properties;
    }

    public Properties getProperties() {
        return properties;
    }

    /**
     * @param string
     */
    static private void trace(String string) {
        if (false)
            Debug.debug(string);
    }

    /**
     * Parses a query and returns the solution in a Hashtable. The keys in the
     * Hashtable represent the variables names (java.lang.String), the values
     * are objects of the jpl package (Atom, Compound, jpl.Integer,etc.).
     * 
     * @param query The dot in the end of the Prolog query is not mandatory, but may be used.
     * @return if the solution is no the return value is null, else it returns
     *               bindings for the variables in an Hashtable. The Hashtable is
     *               empty if no variables are used in the query.
     * @see java.lang.Hashtable
     */
    public Hashtable query(String query) {
    	if(query == null || query.length() == 0)
    		return null;
    	if(query.charAt(query.length()-1) =='.')
    		query = query.substring(0, query.length()-1);

    	while (isLocked()) {
            //System.err.println("DEBUG: olala, i'm locked.");
            try {
                //FIXME:ld: i hope this assumption remains true:
                //notifyAll is called on callLock in Mutex.release()
                waitOnCallLock(100);

            } catch (InterruptedException e) {
                Debug.report(e);
            }
        }
        //System.err.println("DEBUG: hooray, i'm not locked.");

        setQueryActive(true);
        processing = true;
        Hashtable solution;

        if (isHidden())
            solution = (Hashtable) catchedCall(ROLE, "queryNoInteraction",
                    new Object[] { query });
        else
        solution = (Hashtable) catchedCall(ROLE, "query",
                new Object[] { query });
        processing = false;
        if (!isQueryActive()) {
            return null;
        }
        if (solution == null) {
        	setQueryActive(false);
            sendNoResultToListeners();
            return null;
        }
        if (solution.size() == 0) {
            sendYesResultToListeners();
            setQueryActive(false);
            return solution;
        }
        return solution;
    }

    /**
     *  
     */
    private void sendNoResultToListeners() {
        if (!hidden)
            manager.sendNoResultToListeners();
    }

    /**
     *  
     */
    private void sendYesResultToListeners() {
        if (!hidden)
            manager.sendYesResultToListeners();
    }

    public Hashtable next() {
        Hashtable solution = (Hashtable) catchedCall(ROLE, "next",
                new Object[0]);
        if (isQueryActive() && solution == null) {
            //			pollingOff();
            sendNoResultToListeners();
        }
        if (solution == null)
            setQueryActive(false);
        return solution;
    }

 
    /**
     * @return Returns the inCall.
     */
    public boolean isInCall() {
        synchronized(callLock){
            return inCall;
        }
    }

    //	/**
    //	 * use PDTPlugin#getHiddenPrologManager()
    //	 *
    //	 * @return
    //	 */
    //	public static PrologManagerClient getHiddenInstance() throws IOException
    // {
    //		PrologManagerClient inst = getInstance();
    //		synchronized (hiddenInstMonitor) {
    //			while(!inst.isInitdone()) {
    //				
    //			}
    //				
    //			if (inst == null || !inst.isInitdone())
    //				throw new RuntimeException("Main Instance not initialized yet.");
    //			synchronized(shutdownMonitor){
    //				if(inst.shutdown)
    //					throwIllegalServerStateException();
    //			}
    //			if (hiddenInst != null)
    //				return hiddenInst;
    //			hiddenInst = new PrologManagerClient();
    //			hiddenInst.connectToServer();
    //			return hiddenInst;
    //		}
    //	}
    /**
     *  
     */
    private static void throwIllegalServerStateException() {
        throw new IllegalStateException("Client already connected.");
    }

    //	/**
    //	 * @return
    //	 */
    //	public static IPrologManager getHiddenBuilderPrologManager()
    //			throws IOException {
    //		synchronized (builderInstMonitor) {
    //			if (inst == null || !inst.isInitdone())
    //				throw new RuntimeException("Main Instance not initialized yet.");
    //			synchronized(shutdownMonitor){
    //				if(inst.shutdown)
    //					throwIllegalServerStateException();
    //			}
    //			if (builderInst != null)
    //				return builderInst;
    //			builderInst = new PrologManagerClient();
    //			builderInst.connectToServer();
    //			return builderInst;
    //		}
    //	}

    public boolean isQueryActive() {
    	synchronized(queryActiveLock) { 
    		return queryActive;
    	}
    }

    /**
     *  
     */
    public void abort() {
        sendAbortInfoToListeners();
        setQueryActive(false);
        //		pollingOff();
    }

    /**
     *  
     */
    private void sendAbortInfoToListeners() {
        if (!hidden)
            manager.sendAbortInfoToListeners();
    }

    void unconnectClient() {
        synchronized (shutdownMonitor) {
            shutdown = true;
        }
    }

    public void addPrologListener(IPrologListener listener) {
    	synchronized (prologListeners) {
    	    if(!prologListeners.contains(listener)){
    	        prologListeners.add(listener);
    	    }
    	}
    }

    public void removePrologListener(IPrologListener listener) {
    	synchronized (prologListeners) {
    	    if(prologListeners.contains(listener)){
    	        prologListeners.remove(listener);
    	    }
    	}
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologClient#isHidden()
     */
    public boolean isHidden() {      
        return hidden;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologClient#consult(java.lang.String)
     */
    public void consult(String tmpPathName) {
        query("consult('"+ PrologHelper.makeFilenameSWIConform(tmpPathName)+ "')");
    }

}