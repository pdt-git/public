/*
 * Created on 25.05.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.prolog;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.ServerSocket;
import java.net.SocketException;
import java.net.URL;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPerspective;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.PDTPreferencePage;
import org.cs3.pl.PrologStreamReader;
import org.cs3.pl.SystemProperties;
import org.cs3.pl.exceptions.ExceptionHandler;
import org.cs3.pl.extension.IJTransformerObserver;
import org.cs3.pl.interaction.InteractionHandler;
import org.eclipse.core.resources.IProject;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.ServiceNotFoundException;
import org.rapla.components.rpc.TimeoutException;
import org.rapla.components.rpc.TooManyConnectionsException;

/**
 */
public class PrologManager {

	private static CacheOutputPrologListener cache;
	private static Object hiddenInstMonitor = new Object();
	private static Object builderInstMonitor = new Object();
	private static Object baseInstMonitor = new Object();
	private static PrologClient baseClient = null;
	private static PrologClient hiddenInst = null;
	private static PrologClient builderInst = null;
	private static boolean baseStarted = false;
	private BufferedWriter prologWriter;
	public static final int NO_STREAM=-1;
	public static final int OUT_STREAM = 0;
	public static final int ERR_STREAM = 1;
	public static final int IN_STREAM = 2;	
	private Process prologServer;
	private ThreadedReader errPrinter;
	private ThreadedReader inPrinter;
	//a set cannot be cloned, (it's an interface) a hash set can.
	HashSet prologListeners = new HashSet();
	static private PrologManager managerInst; 
	int init_counter = 0;
	private static int PORT = 8807;
	public int port = getServerPort();
	private boolean showOutput = false;
	final private IPrologListener systemStreams = new SystemStreamsPrologListener();
	
	/*
	 * ld: i added this for debugging.
	 * if set to true, the server is expected to be up and listeneing on port PORT
	 * No attempt will be made to start or stop it
	 * 
	 * this is rather pre-alpha, don't expect too much. ;-)
	 * in particular, restarting does not work as you might expect.
	 * TODO: this should be a regular supported option in some futur release! 
	 *  
	 * To avoid further fuzz (e.g. due to accidently commiting this flag set to true ;-) )
	 * I suggest the following solution:
	 *  By explicitly setting the system property "org.cs3.pl.jtransformer.standalone_server"
	 * to a value of "true", the option is enabled, otherwise, it is disabled.
	 * Thus, the behaviour is workspace-dependend, i.e., it will not accidently end up in
	 * cvs.
	 * 
	 * FYI, setting a System Property can be acomplished by passing the following argument
	 * to the vm:
	 * -Dorg.cs3.pl.jtransformer.standalone_server=true
	 */
	private static final boolean STANDALONE_SERVER = Boolean.getBoolean("org.cs3.pl.jtransformer.standalone_server");
	

	private static final Object getInstanceMonitor = new Object();
	
	private IPrologListener dispatcher = new IPrologListener(){

        public void enterCatchedCall(PrologEvent e) {
            fireEnterCatchedCall(e);
            
        }

        public void exitCatchedCall(PrologEvent e) {
            fireExitCatchedCall(e);
            
        }

        public void newDataAvailable(PrologEvent e) {
            //ld: with the current implementation, this will never be called from here.
            fireNewDataAvailable(e);
            
        }
	    
	};

	public static PrologManager getInstance() {
	    if (managerInst == null) {
            synchronized (getInstanceMonitor) {
                try {
                    managerInst = new PrologManager();
                } catch (IOException e) {
                    e.printStackTrace();
                    Debug.report(e);
                }
            }
        }
        return managerInst;
	}


	private PrologManager() throws IOException{
		Runtime thisRuntime = Runtime.getRuntime();
		cache = new CacheOutputPrologListener();
		addPrologListener(cache);
		initServer();				
		thisRuntime.addShutdownHook(new Thread() {
			public void run() {
				halt();
			}
		});
	}
	
	/**
	 * @return Returns the cache.
	 */
	public CacheOutputPrologListener getCache() {
		CacheOutputPrologListener tmp = cache;
		return tmp;
	}

	public IPrologClient getClient() {
		ensureBaseStarted();
		return baseClient;
	}
	
	/**
     * @return
     */
    private void ensureBaseStarted() {
        synchronized (baseInstMonitor) {
			if (baseStarted)
				return ;
			try {
				connectClients();
				initFactbase();
				if (port != getServerPort()){
				    InteractionHandler.tell("Old server still running. Started new one.","Server Initialization");
				}				    
//					PDTPlugin.getDefault().getDisplay().asyncExec(
//							new Runnable() {
//								public void run() {
//									MessageDialog
//											.openInformation(PDTPlugin
//													.getShell(),
//													"Server Initialization",
//													"Old server still running. Started new one.");
//								}
//							});
			} catch (final FileNotFoundException fnfe) {
			    ExceptionHandler.handle(fnfe);
//				PDTPlugin.getDefault().getDisplay().asyncExec(new Runnable() {
//					public void run() {
//						PDTPlugin
//								.message("could not initialize Prolog Server: \n"
//										+ fnfe.getLocalizedMessage());
//					}
//				});

			} catch (IOException e) {
				ExceptionHandler.handle(e);
//				PDTPlugin.getDefault().getDisplay().asyncExec(new Runnable() {
//					public void run() {
//						MessageDialog.openError(PDTPlugin.getShell(),
//								"Server Initialization",
//								"Old server still running.");
//						Runtime rt = Runtime.getRuntime();
//						try {
//							Process process = rt.exec("pskill");
//							try {
//								process.waitFor();
//							} catch (InterruptedException e1) {
//								Debug.report(e1);
//							}
//						} catch (IOException e) {
//							Debug.report(e);
//						}
//
//						PDTPlugin.getDefault().getWorkbench().close();
//					}
//				});
			}
			baseStarted = true;
			
		}
    }


    public IPrologClient getHiddenClient() throws IOException {
		synchronized (hiddenInstMonitor) {
			ensureBaseStarted();
			if (hiddenInst != null)
				return hiddenInst;
			hiddenInst = new PrologClient(this,true);
			hiddenInst.addPrologListener(dispatcher);
			hiddenInst.connectToServer();
			return hiddenInst;
		}
	}

	/**
	 * @return
	 */
	public IPrologClient getHiddenBuilderClient() throws IOException {
		synchronized (builderInstMonitor) {
		    ensureBaseStarted();
			if (builderInst != null)
				return builderInst;
			builderInst = new PrologClient(this,true);
			builderInst.addPrologListener(dispatcher);
			builderInst.connectToServer();
			return builderInst;
		}
	}
	
	public void addPrologListener(IPrologListener listener) {
		synchronized (prologListeners) {
			prologListeners.add(listener);
		}
	}
	public void removePrologListener(IPrologListener listener) {
		synchronized (prologListeners) {
			prologListeners.remove(listener);
		}
	}
	void updatePrologListeners(int kind, String data) {
		fireNewDataAvailablel(data,kind);
	}

	
	public void connectClients() throws IOException {
		
		baseClient = new PrologClient(this,false);
		baseClient.addPrologListener(dispatcher);

		Debug.debug("initialize Prolog Manager Server (" + ++init_counter + ")");
		if (baseClient.isRunning())
			throwIllegalServerStateException();
		baseClient.configure("localhost", port);
		baseClient.enableLogging(new Logger("client"));
		//client.getLogger().setDebug(false);
		Debug.info("starting client, looking for server at port "+port);
		long millis = 500;
		do {
//			try {				
				baseClient.start();
//			} catch (IOException e1) {
//				//Debug.report(e1);
//				//FIXME: do we have no other means to handle this?
//				//is there an easy way for the _server_  to notify _us_ 
//				//as soon as it is up and running?
//				if(e1 instanceof TooManyConnectionsException)
//					Debug.error("Can not connect client to server: Too many connections to the server.");
//				else
//					Debug.info("server not up yet, sleeping "+millis+" millis.");
//
//				try {
//					//for now let's sleep (not wait, we WANT to keep all monitors locked!)
//					Thread.sleep(millis);
//				} catch (InterruptedException e2) {
//					Debug.report(e2);
//				}
//			}
		} 
		while (!baseClient.isRunning() && !STANDALONE_SERVER);
		if(STANDALONE_SERVER && !baseClient.isRunning()){
			Debug.warning("server did NOT start.");
		}
		else {
			Debug.info("server seems to be running.");
		}
		
		if(builderInst != null)
			builderInst.reconnect();
		if(hiddenInst != null)
			hiddenInst.reconnect();

		try {
			baseClient.call(PrologClient.ROLE, "consult", 
			        new String[]{getEngineDir()+"main.pl"});
		} catch (Exception e) {
			Debug.report(e);
		}
	}
	
	
	/**
	 * @param client
	 * @throws IOException
	 */
	private void initFactbase() throws IOException {
		if (PDTPlugin.getDefault() != null) {
		    //baseClient.query("prolog_server(8888,[])");
			Hashtable solution = baseClient.query("catch(set_outdir('"
					+ PrologHelper.makeFilenameSWIConform(PDTPlugin
							.getDefault().getWorkspaceLocation()
							+ PDTPlugin.TRANSFORMED) + "'),PredicateNotDefinedException,true)");
			if(!solution.get("PredicateNotDefinedException").toString().startsWith("_"))
			    Debug.error("The predicate 'set_outdir/1' is not defined yet: "+ solution.get("PredicateNotDefinedException").toString());
			FactBaseInitialization initfactbase = new FactBaseInitialization(baseClient);
			initfactbase.onStartup();
			System.out.println("after fact base init startup");
			if (!initfactbase.isCompleteFactbaseLoaded())
			    PDTPlugin.getDefault().initializeOpenProjects();
			
            PDTPlugin.getDefault().updateFactbaseObservers(
               IJTransformerObserver.JT_ENGINE_STARTUP, baseClient,null);
            
            if (initfactbase.isCompleteFactbaseLoaded())
                PDTPlugin.getDefault().updateFactbaseObservers(
                		IJTransformerObserver.JT_FACTBASE_UPDATED, baseClient,
						PDTPlugin.getDefault().getFirstProjectWithJLMPNature());
		}
		baseClient.setQueryActive(false);
	}


	private void initServer() throws IOException {
		if(STANDALONE_SERVER) port = getServerPort();
		else port = getFreeServerSocket(getServerPort());
		if (port != getServerPort()) {
			killServerProcess();
        	port = getFreeServerSocket(getServerPort());
		}
		
		String prologBin;
		String classPath;
		String projectDir = getProjectDir();
		
		SystemProperties sp = new SystemProperties();

		if (projectDir.charAt(projectDir.length() - 1) != File.separatorChar)
			projectDir += File.separator;
		
		String jtransformerJar = projectDir + "jtransformer.jar";
		if ((new File(jtransformerJar)).exists()) { // plugin deployed
			classPath = jtransformerJar;
		} else {
			classPath = projectDir + "bin" + java.io.File.separator;
		}
		String storeClasspath = PDTPlugin.getDefault().getPreferenceStore().getString(PDTPreferencePage.P_PROLOG_CLASSPATH);
		if (storeClasspath != null && storeClasspath.length() > 0) 
		    classPath +=File.pathSeparator + storeClasspath;

		//classPath +=File.pathSeparator + System.getProperty("java.class.path");
		
//		engineDir = getEngineDir(); 
		
		if (sp.isWindowsPlattform()){
			prologBin = projectDir + "swipl" + File.separator + "bin";
			if (!(new File(prologBin)).exists())
				throw new FileNotFoundException(
						"Swi Prolog could not be found in the expected directory:\n"
								+ prologBin);
			
			startServer(port, prologBin, classPath);
		} else if (sp.isLinuxPlatform()){ 
			Debug.debug("Linux startup");
			
			startServer(port, ".", classPath);
			
		} else {
			String arch = System.getProperty("os.name", "Unknown");
			Debug.error("Unsupported Architecture: "+ arch);
			throw new UnsupportedOperationException("Architecture" + arch + "no supported (yet).");
		}
		
		Debug.debug("PDT directory: " + classPath);
		Debug.debug("starting new server");
	}
	/**
	 * Auxiliary method used in the initialization of the server
	 * process. If the server is already running (hopefully abandoned)
	 * its process will be killed.
	 * 
     * @throws IOException
     * @return true, if the server process has been killed
     */
    boolean killServerProcess() throws IOException {
        PrologClient stopServerClient = new PrologClient(this,false);
        stopServerClient.enableLogging(new Logger("stopClient"));
        Debug.debug("kill running server process");
        stopServerClient.configure("localhost", getServerPort());
        stopServerClient.start();
        if(!stopServerClient.isRunning()){
            Debug.debug("could not connect to server");
            return false;
        }
        try {
//        	try {
        		stopServerClient.call(PrologClient.ROLE,"systemExit",new Object[0]);
//        	}
//        	catch( SocketException se) {
//        		Debug.debug("killed server process");
//        		return true;
//        	}
//        } catch (TimeoutException e) {
//        	throw new IOException(e.getLocalizedMessage());
        } catch (InvocationTargetException e) {
        	throw new IOException(e.getLocalizedMessage());
        } catch (ServiceNotFoundException e) {
        	throw new IOException(e.getLocalizedMessage());
        }
        return false;
    }


    /**
     * Returns the full swi conform (using slashes) path
     * the the JTransformer engine directory.
     * 
	 * @return
	 * @throws FileNotFoundException
	 */
	static public String getEngineDir() throws FileNotFoundException {
		String projectDir = getProjectDir();
		return projectDir.replace('\\', '/') + "engine/";
	}


	/**
	 * @param port2
	 * @return
	 */
	private int getFreeServerSocket(int port) {
		while (true)
			try {
				ServerSocket sock = new ServerSocket(port);
				sock.close();
				return port;
			} catch (IOException e) {
				port++;
			}
	}
	/**
	 * FIXME: LD: what exactly do we expect this method to return? 
	 * it seems to be used for setting the classpath of the 
	 * server process. It is further used to construct the engine path.
	 * what is the engine path? is it a part of the generic prolog interface?
	 * The prolog interface should not depend on any plugin or eclipse in general.
	 * <p>
	 * in its current implementation, the method tries to get a living PDTPlugin instance, and ask it, where it resides.
	 * if no such instance can be found, it asks the ClassLoader for the location of the following resource (attention, voodoo ahead!)
	 * <p>
	 * <code>"org/cs3/pl/prolog/PrologServer.class"</code>
	 * <p>
	 * <b>beware:</b> if you change the behaviour of this method without updating this java doc, 
	 * you will be inpotent for the rest of your sorry life. AND, you will loose all your hair. no joke..
	 * And all your children will fall in love with the Kelly Family. and... :-)  
	 * @return the path to the project dir INCLUDING a trailing path separator.. 
	 * 	
	 *
	 */
	static public String getProjectDir() throws FileNotFoundException {
		try {
			return PDTPlugin.getDefault().getLocation().replace('/',
					File.separatorChar);
		} catch (NullPointerException npe) {
			Debug.warning("i cought a NullPointerException in getProjectDir(), propably because" +
					"this code still excpects eclipse to be running, while it actualy should not depend on " +
					"it."); 
		}
		String resName = "org/cs3/pl/prolog/PrologServer.class";
		URL url = ClassLoader.getSystemClassLoader().getResource(resName);
		String path = url.getPath();
		
		String pdtDir;
		if(System.getProperty("os.name").startsWith("Windows")){ 
		    pdtDir = path.substring(1, path.length() - resName.length());
		}
		else{
			pdtDir = path;
		}
		//now we should have a absolute filesystem path to the PrologServer class file.
		//we have to strip away a substring with the same length of resName + "bin/".length 
		//from the end of pdtDir to reconstruct the project dir.
		int length = pdtDir.length()-/*resName.length()-*/"bin/".length();
		pdtDir=pdtDir.substring(0,length).replace('/',java.io.File.separatorChar);
		
				
		
		//pdtDir = pdtDir.substring(0, pdtDir.length() - "bin".length() - 1);
		//pdtDir = pdtDir.replace('/',File.separatorChar);
		
		return pdtDir;
	}
	

	
	/**
	 * @param thisRuntime
	 * @param port
	 * @param pdtDir
	 * @throws IOException
	 */
	private void startServer(int port, String prologBin, String classPath)
			throws IOException {
		
		if (STANDALONE_SERVER)return;
		
		Runtime thisRuntime = Runtime.getRuntime();
		ServerSocket testBindingPort = new ServerSocket(port);
		testBindingPort.close();
		String debugOption = "-Dorg.cs3.pl.jtransformer.debug_level="+System.getProperty(Debug.JTRANSFORMER_DEBUG_PROPERTY,"ERROR");
		try {
			String exec = "java -classpath " + classPath +" "
					+ debugOption+ " org.cs3.pl.prolog.PrologServer " + port;
			Debug.debug("executing: "+exec);
			prologServer = thisRuntime.exec(
					exec, null,
					new File(prologBin));
			InputStreamReader errRead = new InputStreamReader(prologServer
					.getErrorStream());
			InputStreamReader inRead = new InputStreamReader(prologServer
					.getInputStream());
			BufferedReader err = new BufferedReader(errRead);
			BufferedReader in = new BufferedReader(inRead);
			setPrologWriter(new BufferedWriter(new PrintWriter(prologServer
					.getOutputStream())));
			errPrinter = new ThreadedReader(err, 1, true);
			errPrinter.start();
			inPrinter = new ThreadedReader(in, 2, false);
			inPrinter.start();
		} catch (IOException e) {
			IOException ex = new IOException("Socket already bound, aborting");
			ex.initCause(e);
			throw ex;
		}
	}

	public void setPrologWriter(BufferedWriter prologWriter) {
		synchronized(prologWriter){
			this.prologWriter = prologWriter;
			prologWriter.notifyAll();
		}
	}
	public BufferedWriter getPrologWriter() {
		synchronized(prologWriter){
			while(prologWriter==null){
				try {
					//ld:do NOT sleep (keeps monitors locked!), 
					//instead we wait (release monitors) 
					//setPrologWriter will notify us,
					Debug.debug("prologWriter is null, waiting 100 millis");
					prologWriter.wait(100);
					//System.err.println("DEBUG:prologWriter is null, waiting 100 millis");
				} catch (InterruptedException e) {
					// FIXME: ld:interesting question. what to do?
					Debug.report(e);
				}
			}
		}
//		int i = 0;
//		while (prologWriter == null)
//			try {
//				Thread.sleep(100);
//				i++;
//				trace("" + i);
//			} catch (InterruptedException e) {
//				trace("Error in BufferedWriter");
//				Debug.report(e);
//			}
		return prologWriter;
	}
	public void sendChar(char c) {
		updatePrologListeners(ERR_STREAM, /* "?- " + */"" + c);
		try {
			getPrologWriter().write("" + c);
			getPrologWriter().flush();
		} catch (IOException e) {
			Debug.report(e);
		}
	}
	/**
	 *  
	 */
	public void restart() throws IOException {
        synchronized (baseInstMonitor) {
			halt();
			FactBaseInitialization initfactbase = 
			    new FactBaseInitialization(null);
			initfactbase.deleteProjectFactFile();
			initServer();		
			connectClients();
			initFactbase();
        }
	}


	public void halt() {
		if (errPrinter != null) {
			try {
			 	//PDTPlugin.getDefault().removeBuilders();
				unconnectClients();
			 	baseClient.stop();
				errPrinter.stop();
				inPrinter.stop();
				Debug.debug("kill Prolog Manager Server  ...");
				prologServer.destroy();
				prologServer.waitFor();
				Debug.debug("killed Prolog Manager Server");
			} catch (Exception ex) {
				Debug.report(ex);
			}
		}

	}

	
	private void unconnectClients() {
		baseClient.removePrologListener(dispatcher);
	    baseClient.unconnectClient();
		
		synchronized(hiddenInstMonitor) {
			if (hiddenInst != null) {
			    hiddenInst.removePrologListener(dispatcher);
				hiddenInst.unconnectClient();
			}
		}
		synchronized(builderInstMonitor) {
			if (builderInst != null) {
			    builderInst.removePrologListener(dispatcher);
				builderInst.unconnectClient();
			}
		}
	}
	


	class ThreadedReader extends PrologStreamReader {
		private int kind;
		public ThreadedReader(BufferedReader reader, int kind, boolean errStream) {
			super(reader, errStream);
			this.kind = kind;
		}
		protected void consumeChars(final String chars) {
		   //Debug.debug("Server says: "+chars);
			updatePrologListeners(kind, chars);
		}
	}
	/**
	 *  
	 */
	void sendYesResultToListeners() {
		updatePrologListeners(ERR_STREAM, "\n Yes\n");
	}
	/**
	 *  
	 */
	void sendNoResultToListeners() {
		updatePrologListeners(ERR_STREAM, "\n No\n");
	}
	/**
	 *  
	 */
	void sendAbortInfoToListeners() {
		updatePrologListeners(ERR_STREAM, "\n\n aborted\n");
	}
	/**
	 * 
	 */
	
	private static void throwIllegalServerStateException() {
		throw new IllegalStateException("Client already connected.");
	}

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
	void fireNewDataAvailablel(String s, int stream) {
	    PrologEvent e = new PrologEvent(this,s,stream);
	    fireNewDataAvailable(e);
	}
	void fireNewDataAvailable(PrologEvent e) {
		//ld:hier nicht erforderlich! siehe addPrologListener
	    HashSet cloned = null;
		synchronized (prologListeners) {
		    cloned = (HashSet) prologListeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
            IPrologListener l = (IPrologListener) it.next();
            l.newDataAvailable(e);
        }
	}
	
	public void showOnConsole() {
		showOutput = true;
	}
	
	public void doNotShowOnConsole() {
		showOutput = false;
	}


    /**
     * @return Returns the pORT.
     */
    public static int getServerPort() {
        return PORT;
    }

    /**
     * @return Returns the pORT.
     */
    public static void setServerPort(int port) {
        PORT = port;
    }


	/**
	 * @param level
	 */
	public static void setDebugLevel(int level) {
		if(baseClient != null && baseClient.isRunning()) {
			try {
				baseClient.call(PrologClient.ROLE,"setDebugLevel", new Object[] {new Integer(level)});
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
	}

}