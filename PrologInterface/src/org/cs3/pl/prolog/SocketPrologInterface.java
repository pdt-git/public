package org.cs3.pl.prolog;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.LifeCycleHookWrapper;
import org.cs3.pl.prolog.internal.ReusablePool;
import org.cs3.pl.prolog.internal.socket.ReusableSocket;
import org.cs3.pl.prolog.internal.socket.SocketClient;
import org.cs3.pl.prolog.internal.socket.SocketSession;

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
public class SocketPrologInterface implements IPrologInterface {

    private class InitSession extends SocketSession {
        public InitSession(SocketClient client) throws IOException {
            super(client);
        }

        public void dispose() {
            Debug.error("Trying to dispose the initial session!");
        }

        public void doDispose() {
            super.dispose();
        }
    }

    private class ShutdownSession extends SocketSession {
        public ShutdownSession(SocketClient client) throws IOException {
            super(client);
        }

        public void dispose() {
            Debug.error("Trying to dispose the shutdown session!");
        }

        public void doDispose() {
            super.dispose();
        }
    }

    private final class StartupThread extends Thread {

        private StartupThread(String name) {
            super(name);
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

        public void start() {
            setDaemon(true);
            super.start();
        }
    }

    private static final int DOWN = 0;

    private static final int ERROR = -1;

    private static final int SHUT_DOWN = 3;

    private static final int START_UP = 1;

    private static final int UP = 2;

    private boolean hookFilpFlop = false;

    private HashMap hooks = new HashMap();

    private int port = 9966;

    private boolean serverDown = true;

    private Collection sessions = new LinkedList();

    private boolean standAloneServer = false;

    private ServerStartStrategy startStrategy = new SocketServerStartStrategy("","xpce");

    private StartupThread startupThread;

    private int state = DOWN;

    private Object stateLock = new Object();

    private ServerStopStrategy stopStrategy = new SocketServerStopStrategy();

    private boolean useSessionPooling = true;

    private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;

    public SocketPrologInterface() throws IOException {

        Runtime.getRuntime().addShutdownHook(
                new Thread("Prolog Shutdown Hook") {
                    public void run() {
                        SocketPrologInterface.this.stop();
                    }
                });
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

    public int getPort() {
        return port;
    }

    /**
     * returns an instance of the "default" session. This is actually a call to
     * getSession(Class) with the default class object as an argument, and
     * therefore needs reflection and an (int)-constructor.
     * 
     * @return a new Session Object
     */
    public PrologSession getSession() {
        ReusableSocket socket = null;
        try {
            if (useSessionPooling) {
                socket = (ReusableSocket) pool
                        .findInstance(ReusableSocket.class);
            }
            if (socket == null) {
                socket = new ReusableSocket("localhost", port);
            }
            SocketClient client = new SocketClient(socket);
            client.setPool(pool);
            return new SocketSession(client);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @return Returns the startStrategy.
     */
    public ServerStartStrategy getStartStrategy() {
        return startStrategy;
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
     * @return Returns the useSessionPooling.
     */
    public boolean getUseSessionPooling() {
        return useSessionPooling;
    }

    public boolean isDown() {
        return (getState() == DOWN);
    }

    /**
     * @return Returns the standAloneServer.
     */
    public boolean isStandAloneServer() {
        return standAloneServer;
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

    public void setPort(int port) {
        if (isDown()) {
            this.port = port;
        } else {
            throw new IllegalStateException("Cannot change port while in use.");
        }
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
     * @param useSessionPooling
     *                    The useSessionPooling to set.
     */
    public void setUseSessionPooling(boolean useSessionPooling) {
        this.useSessionPooling = useSessionPooling;
        pool = useSessionPooling ? new ReusablePool() : null;
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
            InitSession initSession = new InitSession(new SocketClient(
                    "localhost", port));
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
            if (startupThread != null) {
                startupThread.join();
            }
            ShutdownSession s = null;
            try {
                s = new ShutdownSession(new SocketClient("localhost", port));
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
            if (pool != null) {
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
    public ServerStopStrategy getStopStrategy() {
        return stopStrategy;
    }
    public void setStopStrategy(ServerStopStrategy stopStrategy) {
        this.stopStrategy = stopStrategy;
    }
}