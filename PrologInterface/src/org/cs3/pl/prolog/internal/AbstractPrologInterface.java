/*
 */
package org.cs3.pl.prolog.internal;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

/**
 * convenience implementation of common infrastructure.
 * <p>
 * Subclasses have to implement getSession().
 */
public abstract class AbstractPrologInterface implements PrologInterface {
    private List bootstrapLibraries = new Vector();

    public List getBootstrapLibraries(){
        return bootstrapLibraries;
    }
    
    public void setBootstrapLibraries(List l){
        this.bootstrapLibraries=l;
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
    
   
    
    private static final int DOWN = 0;

    private static final int ERROR = -1;

    private static final int SHUT_DOWN = 3;

    private static final int START_UP = 1;

    private static final int UP = 2;

    private HookHelper hookHelper = new HookHelper();

    private Collection sessions = new LinkedList();

    private ServerStartAndStopStrategy startAndStopStrategy ;

    private int state = DOWN;

    private Object stateLock = new Object();

    private HashMap listenerLists = new HashMap();

    public AbstractPrologInterface()  {

        Runtime.getRuntime().addShutdownHook(
                new Thread("Prolog Shutdown Hook") {
                    public void run() {
                        AbstractPrologInterface.this.stop();
                    }
                });
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.IPrologInterface#addLifeCycleHook(org.cs3.pl.prolog.LifeCycleHook)
     */
    public void addLifeCycleHook(LifeCycleHook h) {
        hookHelper.addLifeCycleHook(h);
    }

    /**
     * @param hook
     * @param id
     * @param dependsOn
     */
    public void addLifeCycleHook(LifeCycleHook hook, String id,
            String[] dependencies) {
        hookHelper.addLifeCycleHook(hook, id, dependencies);
    }

    /**
     * 
     * override this if your subclass needs special initial Sessions
     * 
     * @param initSession
     *                    a session obtained from getInitialSession()
     */
    protected void disposeInitialSession(PrologSession initSession) {
        initSession.dispose();
    }

    /**
     * overide this if your subclass needs special shutdown sessions.
     * 
     * @param s
     *                    a session obtained from getShutdownSession()
     */
    protected void disposeShutdownSession(PrologSession s) {
        s.dispose();

    }

    /**
     * override this if your subclass needs special initial Sessions
     * 
     * @return
     */
    protected PrologSession getInitialSession() {
        return getSession();
    }

    /**
     * override this if you need configurable options. the default
     * implementation does not have any configuragble options, so it will always
     * through an IllegalArgumentException..
     */
    public String getOption(String opt) {
        throw new IllegalArgumentException("option not supported: " + opt);
    }

    public abstract PrologSession getSession_impl() throws Throwable;
    public PrologSession getSession(){
        try{
            PrologSession s = getSession_impl();
            sessions.add(new WeakReference(s));
            return s;
        }
        catch(Throwable t){
            Debug.report(t);
            return null;
        }
    }

    /**
     * overide this if your subclass needs special shutdown sessions.
     * 
     * @return
     */
    protected PrologSession getShutdownSession() {
        return getSession();
    }

    /**
     * @return Returns the startStrategy.
     */
    public ServerStartAndStopStrategy getStartAndStopStrategy() {
        return startAndStopStrategy;
    }

    /**
     * @return Returns the state.
     */
    public int getState() {
        synchronized (stateLock) {
            return state;
        }
    }

    public boolean isDown() {
        return (getState() == DOWN);
    }

    /**
     * @return
     */
    public boolean isUp() {
        return getState() == UP;
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
     * override this if you need configurable options. the default
     * implementation does not have any configuragble options, so it will always
     * through an IllegalArgumentException..
     */
    public void setOption(String opt, String value) {
        throw new IllegalArgumentException("option not supported: " + opt);
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

    public void start() throws IOException {
        try {
            setState(START_UP);

        } catch (IllegalStateException e) {
            Debug.warning("I will not start: not in DOWN state!");
            return;
        }
        try {
            if (startAndStopStrategy.isRunning(this)) {
                Debug
                        .warning("ahem... the port is in use. \n"
                                + "Trying to connect & shutdown, but this may not work.");
                startAndStopStrategy.stopServer(this, true);
            }
            startAndStopStrategy.startServer(this);
            PrologSession initSession = getInitialSession();
            hookHelper.onInit(initSession);
            disposeInitialSession(initSession);
            setState(UP);
            hookHelper.afterInit();
        } catch (Throwable t) {
            setState(ERROR);
            Debug
                    .error("Could not start PI becouse of unhandled exception. Exception will be rethrown.");
            	Debug.report(t);
            stop();
            throw new RuntimeException(t);
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
            PrologSession s = getShutdownSession();
            if (s != null) {
                hookHelper.beforeShutdown(s);
                try {
                    disposeShutdownSession(s);
                } catch (Throwable t) {
                    Debug.error("could not dispose shutdown session.");
                    Debug.report(t);
                }
            }
            synchronized (sessions) {
                for (Iterator i = sessions.iterator(); i.hasNext();) {
                    WeakReference element = (WeakReference) i.next();
                    PrologSession ps = (PrologSession) element.get();
                    if (ps != null&& !ps.isDisposed())
                        try {
                            ps.dispose();
                        } catch (Throwable t) {
                            Debug.report(t);
                        }
                    i.remove();
                }
            }
            startAndStopStrategy.stopServer(this, true);
            setState(DOWN);
        } catch (Throwable t) {
            setState(ERROR);
            Debug.report(t);
            Debug.error("Could not shut down.");
            throw new RuntimeException(t);
        }
    }
    /**
     * @param startAndStopStrategy The startAndStopStrategy to set.
     */
    public void setStartAndStopStrategy(
            ServerStartAndStopStrategy startAndStopStrategy) {
        this.startAndStopStrategy = startAndStopStrategy;
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
}
