package org.cs3.pl.prolog;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.ref.WeakReference;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.common.Util;
import org.rapla.components.rpc.Logger;

/**
 * Provides a means to start PrologSessions. This class models the lowest layer
 * of our connection to the Prolog Engine, and handles startup and shutdown of
 * the Engine. It also abstracts from any particular Engine implementation,
 * leaving only the getSession() interface to interact with Prolog.
 * 
 * @author terra
 */
public class PrologInterface implements IPrologInterface {
	private static final int DOWN = 0;

	private static final int START_UP = 1;

	private static final int UP = 2;

	private static final int SHUT_DOWN = 3;

	private Object stateLock = new Object();

	private int state = DOWN;

	private boolean useSessionPooling = Boolean
			.getBoolean(Properties.USE_SESSION_POOLING);

	private boolean standAloneServer = Boolean
			.getBoolean(Properties.SERVER_STANDALONE);

	private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;

	private final class StartupThread extends Thread {
		private Map hooks;

		private StartupThread(String name) {
			super(name);
		}

		public void start(Map c) {
			hooks = c;
			setDaemon(true);
			super.start();
		}

		public void run() {
			hookFilpFlop=!hookFilpFlop;
			for (Iterator it = hooks.keySet().iterator(); it.hasNext();) {
				LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks.get(it.next());
				if(h.flipflop!=hookFilpFlop){
					h.afterInit();
				}
			}
		}
	}

	private final class OutputReporterThread extends Thread {
		BufferedReader in;

		Vector listeners;

		private OutputReporterThread(String name) {
			super(name);
		}

		public void run() {

			while (true) {
				try {
					String l = in.readLine();
					if (l == null)
						return;

					Vector list = (Vector) listeners.clone();

					for (Iterator i = list.iterator(); i.hasNext();) {
						OutputListener next = (OutputListener) i.next();
						next.onOutput(l);
					}

				} catch (IOException e) {
					Debug
							.error("IOException caught, Server output reporting shutting down");
					return;
				}
			}
		}

		public void start(Process p, Vector listeners) {
			Debug.info("Starting server Error reporting");
			setDaemon(true);
			in = new BufferedReader(new InputStreamReader(p.getInputStream()));
			this.listeners = listeners;
			start();
		}
	}

	private final class ErrorReporterThread extends Thread {
		BufferedReader in;

		Vector listeners;

		private ErrorReporterThread(String name) {
			super(name);
		}

		public void run() {

			while (true) {
				try {
					String l = in.readLine();
					if (l == null)
						return;

					Vector list = (Vector) listeners.clone();

					for (Iterator i = list.iterator(); i.hasNext();) {
						OutputListener next = (OutputListener) i.next();
						next.onOutput(l);
					}

				} catch (IOException e) {
					Debug
							.error("IOException caught, Server error reporting shutting down");
					return;
				}
			}
		}

		public void start(Process p, Vector listeners) {
			Debug.info("Starting server Error reporting");
			setDaemon(true);
			in = new BufferedReader(new InputStreamReader(p.getErrorStream()));
			this.listeners = listeners;
			start();
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

	private Vector outListeners = new Vector();

	private Vector errListeners = new Vector();

	private Collection sessions = new LinkedList();

	private boolean serverDown = true;

	private Process server = null;

	private ServerStartStrategy startStrategy = new DefaultServerStartStrategy();

	private ServerStopStrategy stopStrategy = new DefaultServerStopStrategy();

	private StartupThread startupThread;

	private HashMap hooks = new HashMap();
	private boolean hookFilpFlop = false;
	
	public PrologInterface() throws IOException {
		
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
						PrologInterface.this.stop();
					}
				});
	}

	

	/**
	 * returns a direct session to the Interface. This kind of session does not
	 * support the generic instantiation a normal getSession would support, but
	 * contains the sendChar() method in its interface Direct sessions, despite
	 * their name, may be less performant then normal sessions, and should only
	 * be used when absolutely necessary.
	 * 
	 * @return a DirectAccessSession
	 */

	private DirectAccessSession getDirectSession() {
		synchronized (this) {
			if(!isUp()){
				throw new IllegalStateException("PrologInterface is not up.");
			}

			if (server == null) {
				Debug
						.error("DirectSession requested, but we do not control the server process. Things will blow up.");
				return null;
			}

			try {
				return new StreamBackedSession(port, server.getOutputStream());
			} catch (IOException e) {
				Debug.report(e);
				return null;
			}
		}
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
	 *            a class object implementing PrologSession
	 * @return a new Session Object
	 * @throws UnsupportedOperationException
	 *             the class could not be instantiated.
	 * 
	 * 
	 */
	//ld: made this private: it's not used right now, and we should not use it anyway.
	private PrologSession getSession(Class class1) {
		if (Modifier.isAbstract(class1.getModifiers()) || class1.isInterface())
			throw new UnsupportedOperationException("argument abstract");

		if (!PrologSession.class.isAssignableFrom(class1))
			throw new UnsupportedOperationException(
					"class does not derive from PrologSession");

		synchronized (this) {

			if(!isUp()){
				throw new IllegalStateException("PrologInterface is not up.");
			}

		}
		try {

			if (useSessionPooling
					&& class1.isAssignableFrom(SimpleSession.class)) {
				ReusableClient c = null;
				synchronized (pool) {
					c = (ReusableClient) pool
							.findInstance(ReusableClient.class);

					if (c != null) {
						Debug
								.debug("reused recycled connection for SimpleSession");
					} else {
						Debug
								.debug("creating new connection for SimpleSession");

						c = new ReusableClient();
						c.enableLogging(new Logger("default"));
						c.configure("localhost", port);
						c.start();
					}
				}
				SimpleSession s = new SimpleSession(c);
				s.setConnectionPool(pool);
				sessions.add(new WeakReference(s));
				return s;
			}

			return (PrologSession) class1.getConstructor(
					new Class[] { int.class }).newInstance(
					new Object[] { new Integer(port) });
		} catch (Exception e) {
			Debug.report(e);

		}

		throw new UnsupportedOperationException(
				"Instantiation failed for this type");
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

	public synchronized void stop() {
		try{
			setState(SHUT_DOWN);
		}catch (IllegalStateException e) {
			Debug.warning("I will not shut down: not in UP state.");
			return;
		}
		try {
			//ld:gotta get in to get out
			startupThread.join();
		} catch (InterruptedException e2) {
			Debug.report(e2);
		}
		ShutdownSession s=null;
		try {
			s = new ShutdownSession(port);
		} catch (IOException e1) {
			Debug.report(e1);
		}
		
		hookFilpFlop=!hookFilpFlop;
		for (Iterator it = hooks.keySet().iterator(); it.hasNext();) {
			LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks.get(it.next());
			if(h.flipflop!=hookFilpFlop){
				h.beforeShutdown(s);
			}
		}
		
		s.doDispose();
		
		synchronized (sessions) {
			for (Iterator i = sessions.iterator(); i.hasNext();) {
				WeakReference element = (WeakReference) i.next();
				PrologSession ps = (PrologSession) element.get();

				if (ps != null)
					ps.dispose();

				i.remove();
			}
		}
		if (standAloneServer) {
			Debug
					.info("i will not try to stop the server, since its running in stand-alone mode.");
		} else {
			stopStrategy.stopServer(port);
		}
	}

	/**
	 * @return
	 */
	public boolean isUp() {
		return getState() == UP;
	}

	public void start() throws IOException {
		try{
			setState(START_UP);
		
		}catch (IllegalStateException e) {
			Debug.warning("I will not start: not in DOWN state!");
			return;
		}
		
		if (standAloneServer) {
			Debug
					.info("i will not try to start the server, since its running in stand-alone mode.");
		} else {
			if (Util.probePort(port, "" + (char) -1)) {
				Debug
						.warning("ahem... the port is in use. \n"
								+ "Trying to connect & shutdown, but this may not work.");
				stopStrategy.stopServer(port);
			}
			server = startStrategy.startServer(port);

			new ErrorReporterThread("Server Error Reporter").start(server,
					errListeners);
			new OutputReporterThread("Server Output Reporter").start(server,
					outListeners);

		}
		InitSession initSession = new InitSession(port);

		hookFilpFlop=!hookFilpFlop;
		for (Iterator it = hooks.keySet().iterator(); it.hasNext();) {
			LifeCycleHookWrapper h = (LifeCycleHookWrapper) hooks.get(it.next());
			if(h.flipflop!=hookFilpFlop){
				h.onInit(initSession);
			}
		}
		initSession.doDispose();		
		setState(UP);
		
		startupThread = new StartupThread("PrologInterface startup Thread");

		startupThread.start(hooks);
	}

	/**
	 * adds a listener to the Prolog output stream. All output will be forwarded
	 * to the listner objects.
	 * 
	 * @param out
	 *            an OutputListener instance.
	 */

	public void addOutputListener(OutputListener out) {
		outListeners.add(out);
	}

	/**
	 * removes a listener to the Prolog output stream.
	 * 
	 * @param out
	 *            an OutputListener instance.
	 */

	public void removeOutputListener(OutputListener out) {
		outListeners.remove(out);
	}

	/**
	 * adds a listener to the Prolog error stream. All output will be forwarded
	 * to the listner objects.
	 * 
	 * @param out
	 *            an OutputListener instance.
	 */

	public void addErrorListener(OutputListener err) {
		errListeners.add(err);
	}

	/**
	 * removes a listener from the Prolog Error stream.
	 * 
	 * @param an
	 *            Outputlistener.
	 */

	public void removeErrorListener(OutputListener err) {
		errListeners.remove(err);
	}

	/**
	 * @return Returns the startStrategy.
	 */
	public ServerStartStrategy getStartStrategy() {
		return startStrategy;
	}

	/**
	 * @param startStrategy
	 *            The startStrategy to set.
	 * @throws IllegalStateException
	 *             if the PI is already in use, i.e. getInstance() has allready
	 *             been called.
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
	 *            The standAloneServer to set.
	 */
	public void setStandAloneServer(boolean standAloneServer) {
		if(isDown()){
			this.standAloneServer = standAloneServer;		
		}
		else{
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
	 *            The state to set.
	 */
	protected void setState(int newState) throws IllegalStateException{
		synchronized (stateLock) {
			if(state==newState){
				return;
			}
			//check if the transition is allowed.
			switch (newState) {
			case DOWN:
				if (this.state!=SHUT_DOWN){
					throw new IllegalStateException("transition not allowed.");
				}
				Debug.info("PLIF DOWN");
				break;
			case START_UP:
				if (this.state!=DOWN){
					throw new IllegalStateException("transition not allowed.");
				}
				Debug.info("PLIF START_UP");
				break;
			case UP:				
				if (this.state!=START_UP){
					throw new IllegalStateException("transition not allowed.");
				}
				Debug.info("PLIF UP");
				break;
			case SHUT_DOWN:
				if (this.state!=UP){
					throw new IllegalStateException("transition not allowed.");
				}
				Debug.info("PLIF SHUT_DOWN");
				break;
			default:
				throw new IllegalArgumentException("Illegal state:"+newState);
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
	 *            The useSessionPooling to set.
	 */
	public void setUseSessionPooling(boolean useSessionPooling) {
		this.useSessionPooling = useSessionPooling;
	}

	public boolean isDown() {
		return (getState() == DOWN);
	}
	public int getPort() {
		return port;
	}
	public void setPort(int port) {
		if(isDown()){
			this.port = port;			
		}
		else{
			throw new IllegalStateException("Cannot change port while in use.");
		}
	}



	
	
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.IPrologInterface#addLifeCycleHook(org.cs3.pl.prolog.LifeCycleHook)
	 */
	public void addLifeCycleHook(LifeCycleHook h) {
		addLifeCycleHook(h,null,null);		
	}	


	/**
	 * @param hook
	 * @param id
	 * @param dependsOn
	 */
	public void addLifeCycleHook(LifeCycleHook hook, String id, String[] dependencies) {
		if(id==null){
			id="<<"+hooks.size()+">>";
		}
		if(dependencies==null){
			dependencies=new String[0];
		}
		
		LifeCycleHookWrapper node =(LifeCycleHookWrapper) hooks.get(id);
		if( node==null){
			node = new LifeCycleHookWrapper(hook,id);
			hooks.put(id,node);
		}
		for (int i = 0; i < dependencies.length; i++) {
			LifeCycleHookWrapper dep = (LifeCycleHookWrapper) hooks.get(dependencies[i]);
			if(dep==null){
				dep=new LifeCycleHookWrapper(null,dependencies[i]);
				hooks.put(dependencies[i],node);
			}
			dep.pre.add(node);
			node.post.add(node);
		}
		
	}
}