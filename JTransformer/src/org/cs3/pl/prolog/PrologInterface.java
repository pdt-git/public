package org.cs3.pl.prolog;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;

import java.lang.ref.WeakReference;
import java.lang.reflect.Modifier;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.SystemProperties;


/**
 * Provides a means to start PrologSessions. This class models the lowest layer of our connection to the Prolog
 * Engine, and handles startup and shutdown of the Engine. It also abstracts from any particular Engine implementation,
 * leaving only the getSession() interface to interact with Prolog.
 * 
 * @author terra
 */
public class PrologInterface {

	private class InitSession extends SimpleSession {
		public InitSession(int PORT) throws IOException {
			super(PORT);
		}

		public void dispose() {
			Debug.error("Trying to dispose the initial session!");
		}
		
		public void doDispose(){
			super.dispose();
		}
	}
	
	private static PrologInterface instance = null;
	
	private static Set initHooks = new HashSet();
	private static Set startHooks =  new HashSet();

	private static final int PORT = 9966;
	
	private Collection sessions = new LinkedList();

	private boolean serverDown = true;
	
	private Process server = null;
	
	private PrologInterface() throws IOException{
		start();
		Runtime.getRuntime().addShutdownHook(new Thread("Prolog Shutdown Hook") {
			public void run(){
				PrologInterface.this.stop();
			}
		});
	}
	
	/**
	 * returns the instance of the PrologInterface. Can cause init and startup hooks to be called. All
	 * initHooks and StartupHooks should be registered before the first call to this method, but will 
	 * be accepted later (for restart() calls).
	 * @return an instance of this class
	 */
	
	public static PrologInterface getInstance() {
		synchronized (PrologInterface.class){
			if (instance == null){
				try {
					instance = new PrologInterface();
				} catch (IOException e) {
					Debug.report(e);
				}	
			}
		}
		
		return instance;
	}
	
	/**
	 * returns a direct session to the Interface. This kind of session does not support the generic 
	 * instantiation a normal getSession would support, but contains the sendChar() method in its interface
	 * Direct sessions, despite their name, may be less performant then normal sessions, and should only
	 * be used when absolutely necessary. 
	 * @return a DirectAccessSession
	 */
	
	public DirectAccessSession getDirectSession(){
		synchronized (this) {
			while (serverDown) {
				try {
					wait();
				} catch (InterruptedException e1) {
					Debug.report(e1);
				}
			}

			try {
				return new StreamBackedSession(PORT, server.getOutputStream(), server.getInputStream());
			} catch (IOException e) {
				Debug.report(e);
				return null;
			}
		}
	}

	/**
	 * returns an instance of the "default" session. This is actually a call to getSession(Class) with the
	 * default class object as an argument, and therefore needs reflection and an (int)-constructor.
	 * @return a new Session Object
	 */
	public PrologSession getSession() {
		return getSession(SimpleSession.class);
	}
		
	/**
	 * returns an instance of a Session class. The class argument must be a concrete class deriving from 
	 * PrologSession, and offer an constructor that takes a single int argument. The constructor will be called
	 * with the PrologServer's port.
	 * 
	 * @param classObject a class object implementing PrologSession
	 * @return a new Session Object
	 * @throws UnsupportedOperationException the class could not be instantiated.
	 */
	public PrologSession getSession(Class class1) {
		if (Modifier.isAbstract(class1.getModifiers()) || class1.isInterface())
			throw new UnsupportedOperationException("argument abstract");
		
		if (!PrologSession.class.isAssignableFrom(class1))
			throw new UnsupportedOperationException("class does not derive from PrologSession");
		
		
		synchronized (this) {
			while (serverDown) {
				try {
					wait();
				} catch (InterruptedException e1) {
					Debug.report(e1);
				}
			}

			try {
				return (PrologSession) class1.getConstructor(new Class[] { int.class }).newInstance(new Object[] { new Integer(PORT) });
			} catch (Exception e) {
				Debug.report(e);
			}
		}
		
		throw new UnsupportedOperationException("Instantiation failed for this type");
	}
	
	

	/**
	 * adds a system initialization callback. The method onInit() is called from each of them, with a specific
	 * initialization session. They are all executed, synchronously, before any other access is allowed.
	 * @param i an InitHook instance
	 */
	public static void addInitHook(InitHook i) {
		synchronized (PrologInterface.class){
			if (instance != null)
				Debug.warning("Adding an initHook after instance was created");
		}
		
		synchronized (initHooks){
			initHooks.add(i);
		}
		
	}

	/**
	 * adds a startup callback. These callbacks are called asynchronously after system 
	 * initialization.
	 * @param i an StartupHook instance.
	 */
	public static void addStartupHook(StartupHook i) {
		synchronized (startHooks) {
			startHooks.add(i);
		}		
	}

	/**
	 * causes complete re-initialization of the Prolog system, and invalidates all current 
	 * sessions.
	 * @throws IOException
	 */
	public void restart() throws IOException {
		stop();
		start();
	}
	
	synchronized void stop() {
		serverDown = true;
		
		synchronized(sessions){
			for (Iterator i = sessions.iterator(); i.hasNext();) {
				WeakReference element = (WeakReference) i.next();
				PrologSession s = (PrologSession) element.get();
				
				if (s != null)
					s.dispose();
				
				i.remove();			
			}
		}
		
		server.destroy();
		
		try {
			server.waitFor();
		} catch (InterruptedException e) {
			Debug.report(e);
		}
		
		server = null;
	}

	private void start() throws IOException {
		synchronized (this){
			String dir = new SystemProperties().isWindowsPlattform()? "swipl\\bin": ".";
			String cmdline = "java -classpath " 
				+ PDTPlugin.getClasspath() 
				+ " org.cs3.pl.prolog.PrologInterfaceServer " 
				+ PORT;
			Debug.debug("Starting server with " + cmdline);
			server = Runtime.getRuntime().exec(cmdline, null, new File(dir));		
			
			new Thread("Server Error Reporter"){
				BufferedReader in;
				
				public void run(){
					
					while (true){
						try {
							String l = in.readLine();
							if (l == null)
								return;
							
							Debug.debug("Server: " + l);
							
						} catch (IOException e) {
							Debug.report(e);
							return;
						}
					}
				}
				
				public void start(Process p){
					Debug.info("Starting server Error reporting");
					setDaemon(true);
					in = new BufferedReader(new InputStreamReader(p.getErrorStream()));
					start();
				}
				
			}.start(server);
			
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				// if interrupted, the system thinks we have waited long enough ;)
				Debug.report(e);
			}
			
			InitSession initSession = new InitSession(PORT);
			
			for (Iterator i = initHooks.iterator(); i.hasNext();) {
				InitHook h = (InitHook) i.next();
				
				h.onInit(initSession); 
			}
			
			initSession.doDispose();
			
			serverDown = false;
			notifyAll();
		}
		
		new Thread("PrologInterface startup Thread"){
			private Collection hooks;
			
			public void start(Collection c){
				hooks = c;
				setDaemon(true);
				super.start();
			}
			
			public void run(){
				synchronized(hooks){
					for (Iterator i = hooks.iterator(); i.hasNext();) {
						StartupHook element = (StartupHook) i.next();
						element.onStartup();
					}
				}
			}
		}.start(startHooks);
	}
}
