package org.cs3.pl.prolog.internal.rpc;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.DefaultConsultService;
import org.cs3.pl.prolog.internal.LifeCycleHookWrapper;
import org.cs3.pl.prolog.internal.ReusablePool;
import org.cs3.pl.prolog.internal.ServerStartStrategy;
import org.cs3.pl.prolog.internal.ServerStopStrategy;
import org.rapla.components.rpc.Logger;

/**
 * Provides a means to start PrologSessions. This class models the lowest layer
 * of our connection to the Prolog Engine, and handles startup and shutdown of
 * the Engine.
 * <p>
 * <i>The api provided by this class is intended to be accessed by code that
 * wants to provide an IPrologInterface service. It contains implementation
 * specific configuration details that a regular client should not temper with.
 * A regular client only see the IPrologInterface api.
 * <p>
 * Any suggestions on how to enforce this are apreciated:
 * degenerl_AT_cs_DOT_uni-bonn_DOT_de </i>
 * 
 * @author terra
 */
public class RPCPrologInterface implements PrologInterface {
    private static final int DOWN = 0;

    private static final int START_UP = 1;

    private static final int UP = 2;

    private static final int SHUT_DOWN = 3;

    private static final int ERROR = -1;

    private Object stateLock = new Object();

    private int state = DOWN;

    private boolean useSessionPooling = true;

    private boolean standAloneServer = false;

    private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;
    private HashMap listenerLists = new HashMap();
    private final class StartupThread extends Thread {

        private StartupThread(String name) {
            super(name);
        }

        public void start() {
            setDaemon(true);
            super.start();
        }

        public void run() {
            synchronized (hooks) {
                hookFilpFlop = !hookFilpFlop;
                for (Iterator it = hooks.keySet().iterator(); it.hasNext();) {
                    LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks
                            .get(it.next());
                    if (h.flipflop != hookFilpFlop) {
                        h.afterInit();
                    }
                }
            }
        }
    }

    private class InitSession extends SimpleSession {
        public InitSession(int PORT) throws IOException {
            super(PORT);
        }

        public void dispose() {
            Debug.error("Trying to dispose the initial session!");
        }

        public void doDispose() {
            super.dispose();
        }
    }

    private class ShutdownSession extends SimpleSession {
        public ShutdownSession(int PORT) throws IOException {
            super(PORT);
        }

        public void dispose() {
            Debug.error("Trying to dispose the shutdown session!");
        }

        public void doDispose() {
            super.dispose();
        }
    }

    private int port = 9966;

    private Collection sessions = new LinkedList();

    private boolean serverDown = true;

    private ServerStartStrategy startStrategy = new RPCServerStartStrategy();

    private ServerStopStrategy stopStrategy = new RPCServerStopStrategy();

    private StartupThread startupThread;

    private HashMap hooks = new HashMap();

    private boolean hookFilpFlop = false;

    
    private ResourceFileLocator locator = new DefaultResourceFileLocator().subLocator(".PrologInterface");

    /**
     * @return Returns the locator.
     */
    public ResourceFileLocator getLocator() {
        return locator;
    }
    /**
     * @param locator The locator to set.
     */
    public void setLocator(ResourceFileLocator locator) {
        this.locator = locator;
    }
    public RPCPrologInterface() throws IOException {

        //		String property = System.getProperty(Properties.PLIF_HOOKS);
        //		if(property!=null){
        //			String[] names = property.split(
        //				",");
        //		
        //			for (int i = 0; i < names.length; i++) {
        //				try {
        //					Class.forName(names[i]);
        //				} catch (ClassNotFoundException e) {
        //					Debug.report(e);
        //				}
        //			}
        //		}
        //		//start();
        Runtime.getRuntime().addShutdownHook(
                new Thread("Prolog Shutdown Hook") {
                    public void run() {
                        RPCPrologInterface.this.stop();
                    }
                });
    }

    /**
     * returns an instance of the "default" session. This is actually a call to
     * getSession(Class) with the default class object as an argument, and
     * therefore needs reflection and an (int)-constructor.
     * 
     * @return a new Session Object
     */
    public PrologSession getSession() {
        return getSession(SimpleSession.class);
    }

    /**
     * returns an instance of a Session class. The class argument must be a
     * concrete class deriving from PrologSession, and offer an constructor that
     * takes a single int argument. The constructor will be called with the
     * PrologServer's port.
     * 
     * @param classObject
     *                    a class object implementing PrologSession
     * @return a new Session Object
     * @throws UnsupportedOperationException
     *                    the class could not be instantiated.
     * 
     *  
     */
    //ld: made this private: it's not used right now, and we should not use it
    // anyway.
    private PrologSession getSession(Class class1) {
        if (Modifier.isAbstract(class1.getModifiers()) || class1.isInterface())
            throw new UnsupportedOperationException("argument abstract");

        if (!PrologSession.class.isAssignableFrom(class1))
            throw new UnsupportedOperationException(
                    "class does not derive from PrologSession");

        synchronized (this) {

            if (!isUp()) {
                throw new IllegalStateException("PrologInterface is not up.");
            }

        }
        try {

            if (useSessionPooling
                    && class1.isAssignableFrom(SimpleSession.class)) {
                ReusableRPCClient c = null;
                synchronized (pool) {
                    c = (ReusableRPCClient) pool
                            .findInstance(ReusableRPCClient.class);

                    if (c != null) {
                        Debug
                                .debug("reused recycled connection for SimpleSession");
                    } else {
                        Debug
                                .debug("creating new connection for SimpleSession");

                        c = new ReusableRPCClient();
                        c.enableLogging(new Logger("default"));
                        c.configure("localhost", port);
                        c.start();
                    }
                }
                SimpleSession s = new SimpleSession(c);
                s.setConnectionPool(pool);
                s.setDispatcher(new PrologInterfaceListener() {
                    public void update(PrologInterfaceEvent e) {
                        fireUpdate(e.getSubject(),e.getEvent());
                    }
                });
                sessions.add(new WeakReference(s));
                return s;
            }

            return (PrologSession) class1.getConstructor(
                    new Class[] { int.class }).newInstance(
                    new Object[] { new Integer(port) });
        } catch (Exception e) {
            Debug.report(e);
            throw new RuntimeException("Instantiation failed for this type", e);

        }

    }

    /**
     * causes complete re-initialization of the Prolog system, and invalidates
     * all current sessions.
     * 
     * @throws IOException
     */
    public void restart() throws IOException {
        stop();
        start();
    }
    /**
     * @param subject2
     * @param string
     */
    public void fireUpdate(String subject, String event) {
        Vector listeners = (Vector) listenerLists.get(subject);
        if (listeners == null) {
            return;
        }
        PrologInterfaceEvent e = new PrologInterfaceEvent(this, subject, event);

        Vector cloned = null;
        synchronized (listeners) {
            cloned = (Vector) listeners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            PrologInterfaceListener l = (PrologInterfaceListener) it.next();
            l.update(e);
        }
    }

    public synchronized void stop() {

        boolean dontTakePrisoners = (getState() == ERROR);
        try {
            setState(SHUT_DOWN);
        } catch (IllegalStateException e) {
            Debug.warning("I will not shut down: not in UP state.");
            return;
        }
        try {
            //ld:gotta get in to get out
            if(startupThread!=null){
                startupThread.join();
            }
            ShutdownSession s = null;
            try {
                s = new ShutdownSession(port);
            } catch (Throwable e1) {
                Debug.report(e1);
                Debug
                        .warning("ShutdownSession could not be created, shutdown hooks not executed.");
            }
            if (s != null) {
                synchronized (hooks) {
                    hookFilpFlop = !hookFilpFlop;
                    for (Iterator it = hooks.keySet().iterator(); it.hasNext();) {
                        String id = (String) it.next();
                        LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks
                                .get(id);
                        if (h.flipflop != hookFilpFlop) {
                            try {
                                h.beforeShutdown(s);
                            } catch (Throwable t) {
                                Debug
                                        .error("could not execute 'beforeShutdown' on hook '"
                                                + id + "'");
                                Debug.report(t);
                            }
                        }
                    }
                }

                try {
                    s.doDispose();
                } catch (Throwable t) {
                    Debug.error("could not dispose shutdown session.");
                    Debug.report(t);
                }
            }

            synchronized (sessions) {
                for (Iterator i = sessions.iterator(); i.hasNext();) {
                    WeakReference element = (WeakReference) i.next();
                    PrologSession ps = (PrologSession) element.get();

                    if (ps != null)
                        try {
                            ps.dispose();
                        } catch (Throwable t) {
                            Debug.report(t);
                        }

                    i.remove();
                }
            }
            if(pool!=null){
                pool.clear();
            }
            if (standAloneServer) {
                Debug
                        .info("i will not try to stop the server, since its running in stand-alone mode.");
            } else {
            	
                stopStrategy.stopServer(port, true);

            }
            setState(DOWN);
        } catch (Throwable t) {
            setState(ERROR);
            Debug.report(t);
            Debug.error("Could not shut down.");
            throw new RuntimeException(t);
        }
    }

    /**
     * @return
     */
    public boolean isUp() {
        return getState() == UP;
    }

    public void start() throws IOException {
        try {
            setState(START_UP);

        } catch (IllegalStateException e) {
            Debug.warning("I will not start: not in DOWN state!");
            return;
        }
        try {
            if (standAloneServer) {
                Debug
                        .info("i will not try to start the server, since its running in stand-alone mode.");
            } else {
                if (Util.probePort(port)) {
                    Debug
                            .warning("ahem... the port is in use. \n"
                                    + "Trying to connect & shutdown, but this may not work.");
                    stopStrategy.stopServer(port, true);
                }
                startStrategy.startServer(port);
            }
            Debug.info("ok... trying to connect to port " + port);
            InitSession initSession = new InitSession(port);
            synchronized (hooks) {
                hookFilpFlop = !hookFilpFlop;
                for (Iterator it = hooks.keySet().iterator(); it.hasNext();) {
                    LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks
                            .get(it.next());
                    if (h.flipflop != hookFilpFlop) {
                        h.onInit(initSession);
                    }
                }
            }
            initSession.doDispose();
            setState(UP);

            startupThread = new StartupThread("PrologInterface startup Thread");

            startupThread.start();
        } catch (Throwable t) {
            setState(ERROR);
            Debug
                    .error("Could not start PI becouse of unhandled exception. Exception will be rethrown.");
            stop();
            throw new RuntimeException(t);
        }
    }

    /**
     * @return Returns the startStrategy.
     */
    public ServerStartStrategy getStartStrategy() {
        return startStrategy;
    }

    /**
     * @param startStrategy
     *                    The startStrategy to set.
     * @throws IllegalStateException
     *                    if the PI is already in use, i.e. getInstance() has allready
     *                    been called.
     */
    public void setStartStrategy(ServerStartStrategy startStrategy) {
        if (!isDown())
            throw new IllegalStateException(
                    "Cannot alter ServerStartStrategy once the PI is in use!");
        this.startStrategy = startStrategy;
    }

    /**
     * @return Returns the standAloneServer.
     */
    public boolean isStandAloneServer() {
        return standAloneServer;
    }

    /**
     * @param standAloneServer
     *                    The standAloneServer to set.
     */
    public void setStandAloneServer(boolean standAloneServer) {
        if (isDown()) {
            this.standAloneServer = standAloneServer;
        } else {
            throw new IllegalStateException("Cannot change port while in use.");
        }

    }

    /**
     * @return Returns the state.
     */
    public int getState() {
        synchronized (stateLock) {
            return state;
        }
    }

    /**
     * @param newState
     *                    The state to set.
     */
    protected void setState(int newState) throws IllegalStateException {
        synchronized (stateLock) {
            if (state == newState) {
                return;
            }

            //check if the transition is allowed.
            switch (newState) {
            case ERROR:
                Debug.info("PLIF ERROR");
                break;
            case DOWN:
                if (this.state != SHUT_DOWN) {
                    throw new IllegalStateException("transition not allowed.");
                }
                Debug.info("PLIF DOWN");
                break;
            case START_UP:
                if (this.state != DOWN) {
                    throw new IllegalStateException("transition not allowed.");
                }
                Debug.info("PLIF START_UP");
                break;
            case UP:
                if (this.state != START_UP) {
                    throw new IllegalStateException("transition not allowed.");
                }
                Debug.info("PLIF UP");
                break;
            case SHUT_DOWN:
                if (this.state == UP || this.state == ERROR) {
                    Debug.info("PLIF SHUT_DOWN");
                } else {
                    throw new IllegalStateException("transition not allowed.");
                }
                break;
            default:
                throw new IllegalArgumentException("Illegal state:" + newState);
            }
            this.state = newState;
        }
    }

    /**
     * @return Returns the useSessionPooling.
     */
    public boolean getUseSessionPooling() {
        return useSessionPooling;
    }

    /**
     * @param useSessionPooling
     *                    The useSessionPooling to set.
     */
    public void setUseSessionPooling(boolean useSessionPooling) {
        this.useSessionPooling = useSessionPooling;
        pool = useSessionPooling ? new ReusablePool() : null;
    }

    public boolean isDown() {
        return (getState() == DOWN);
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        if (isDown()) {
            this.port = port;
        } else {
            throw new IllegalStateException("Cannot change port while in use.");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#addLifeCycleHook(org.cs3.pl.prolog.LifeCycleHook)
     */
    public void addLifeCycleHook(LifeCycleHook h) {
        addLifeCycleHook(h, null, null);
    }

    /**
     * @param hook
     * @param id
     * @param dependsOn
     */
    public void addLifeCycleHook(LifeCycleHook hook, String id,
            String[] dependencies) {
        synchronized (hooks) {
            if (id == null) {
                id = "<<" + hooks.size() + ">>";
            }
            if (dependencies == null) {
                dependencies = new String[0];
            }
            Debug.debug("requested to add hook: id=\"" + id
                    + "\", dependencies=\"" + Util.prettyPrint(dependencies)
                    + "\"");

            LifeCycleHookWrapper node = (LifeCycleHookWrapper) hooks.get(id);

            if (node == null) {
                Debug.debug("\t-> hook unknown, new wrapper created.");
                node = new LifeCycleHookWrapper(hook, id);
                node.flipflop = hookFilpFlop;
                hooks.put(id, node);
            } else {
                node.hook = hook;
            }
            for (int i = 0; i < dependencies.length; i++) {
                LifeCycleHookWrapper dep = (LifeCycleHookWrapper) hooks
                        .get(dependencies[i]);
                Debug.debug("\t-> looking up dependency \"" + dependencies[i]
                        + "\"");
                if (dep == null) {
                    Debug.debug("\t\t-> hook unknown, new wrapper created.");
                    dep = new LifeCycleHookWrapper(null, dependencies[i]);
                    dep.flipflop = hookFilpFlop;
                    hooks.put(dependencies[i], dep);
                }
                dep.pre.add(node);
                node.post.add(dep);
                Debug.debug("\t-> edges added.");
            }
        }

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologInterface#setOption(java.lang.String, java.lang.String)
     */
    public void setOption(String opt, String value) {
        // TODO Auto-generated method stub
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologInterface#getOption(java.lang.String)
     */
    public String getOption(String opt) {
        // TODO Auto-generated method stub
        return null;
    }

        
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologInterface#addPrologInterfaceListener(java.lang.String, org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void addPrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        synchronized(listenerLists){
            Vector list = (Vector) listenerLists.get(subject);
            if(list==null){
                list=new Vector();
                listenerLists.put(subject,list);                
            }
            if(!list.contains(l)){
                list.add(l);
            }
        }

    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String, org.cs3.pl.prolog.PrologInterfaceListener)
     */
    public void removePrologInterfaceListener(String subject,
            PrologInterfaceListener l) {
        synchronized(listenerLists){
            Vector list = (Vector) listenerLists.get(subject);
            if(list==null){
                return;
            }
            if(list.contains(l)){
                list.remove(l);                
            }
            
        }

    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologInterface#getConsultService(java.lang.String)
     */
    public ConsultService getConsultService(String prefix) {
        return new DefaultConsultService(this,locator.subLocator(prefix));        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.IPrologInterface#getFactory()
     */
    public PrologInterfaceFactory getFactory() {
        // TODO Auto-generated method stub
        return null;
    }

  
    private List bootstrapLibraries = new Vector();

    public List getBootstrapLIbraries(){
        return bootstrapLibraries;
    }
    
    public void setBootstrapLibraries(List l){
        this.bootstrapLibraries=l;
    }
}