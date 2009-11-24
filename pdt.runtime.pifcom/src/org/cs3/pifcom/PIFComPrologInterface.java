package org.cs3.pifcom;

import java.io.IOException;
import java.io.InputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.pifcom.preferences.PIFComConstants;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.ReusablePool;
import org.eclipse.jface.preference.IPreferenceStore;

public class PIFComPrologInterface extends AbstractPrologInterface {

	DatagramSocket udpSocket = null;
	private ReusablePool pool = null;

	private InetAddress hostAdr;
	
	private ServerStartAndStopStrategy startAndStopStrategy;
	protected Process process;

	public IPreferenceStore preference_store;
	private HashMap<String, String> preferences = new HashMap();

	/************************************************/
	/**** Options [Start] *****/
	/************************************************/
	
	private int tcpMasterPort;
	private int udpMasterPort;
	
//	public void setOption(String opt, String value) {
////		Option[] options = factory.getOptions();
////		for (Option option : options) {
////			if (option.getId().equals(opt)) {
//
//		
//		// REMOVED CHECK IF OPTION EXISTS, should be implemented different
//		
//		this.preferences.put(opt, value);
//		return;
//				
// 
//				
////			}
////		}
////		super.setOption(opt, value);
//	}
	
	protected void setTcpMasterPort(String tcpMasterPort) {
		this.tcpMasterPort = Integer.parseInt(tcpMasterPort);
	}
	protected void setTcpMasterPort(int tcpMasterPort) {
		this.tcpMasterPort = tcpMasterPort;
	}

	protected int getTcpMasterPort() {
		return tcpMasterPort;
	}
	
	protected void setUdpMasterPort(String udpMasterPort) {
		this.udpMasterPort = Integer.parseInt(udpMasterPort);
	}
	protected void setUdpMasterPort(int udpMasterPort) {
		this.udpMasterPort = udpMasterPort;
	}

	protected int getUdpMasterPort() {
		return udpMasterPort;
	}

	public void initOptions(){
		
		
		super.initOptions();
//		super.initOptions(preference_store);
		
		PrologRuntimePlugin plugin = PrologRuntimePlugin.getDefault();
		
		setTcpMasterPort(plugin.overridePreferenceBySystemProperty(PIFComConstants.PIF_PORT_TCP)); 
		setUdpMasterPort(plugin.overridePreferenceBySystemProperty(PIFComConstants.PIF_PORT_UDP));
		
		//this.hidePlwin = preference_store.getBoolean(PREF_HIDE_PLWIN);
		
	
	}


	
	/************************************************/
	/**** Options [End] *****/
	/************************************************/		
	
	public PIFComPrologInterface(String name) {
		super(name);		
		preference_store = PrologRuntimePlugin.getDefault().getPreferenceStore();

	}

	public PIFComPrologInterface() {
		preference_store = PrologRuntimePlugin.getDefault().getPreferenceStore();

	}

	public AsyncPrologSession getAsyncSession_impl(int flags) throws Throwable {
		ReusablePIFComConnection connection = getConnection();
		return new AsynchronousPIFComSession(connection, this, flags);
	}

	private ReusablePIFComConnection getConnection() throws UnknownHostException, IOException {
		ReusablePIFComConnection connection = null;
		if (pool != null) {
			connection = (ReusablePIFComConnection) pool.findInstance(ReusablePIFComConnection.class);
		}
		if (connection == null) {
			connection = new ReusablePIFComConnection(udpSocket, hostAdr, getTcpMasterPort(), pool) {

				protected void error(Throwable e) throws PrologInterfaceException {
					PIFComPrologInterface.this.error(e);

				}

			};
		}
		return connection;
	}

	public PrologSession getSession_impl(int flags) throws Throwable {
		ReusablePIFComConnection connection = getConnection();
		return new PIFComSession(connection, this, flags);
	}


	public ServerStartAndStopStrategy getStartAndStopStrategy() {

		return new ServerStartAndStopStrategy() {

			public boolean isRunning(PrologInterface pif) {
				if (process == null) {
					return false;
				}
				try {
					process.exitValue();
				} catch (IllegalThreadStateException unwichtig) {
					return true;
				}
				return false;
			}

			public Process startServer(PrologInterface pif) throws IOException {
				System.out.println("Starting PIFCOM");
//				initOptions();
				
				if (isRunning(pif)) {
					Debug.warning("Server process appears to be already running, so I will not start it.");
					return process;
				}
				udpSocket = new DatagramSocket(new InetSocketAddress(0));
				
				
				//standalone = Boolean.valueOf(getOption(PrologInterface.PREF_STANDALONE)).booleanValue();
				
				if (isStandAloneServer()) {
					Debug.warning("Will not start server; the option " + PrologInterface.PREF_STANDALONE + " is set.");
					//setHost(getOption(PrologInterface.PREF_HOST));
					hostAdr = InetAddress.getByName(getHost());

					//udpMasterPort = Integer.parseInt(getOption(PIFComConstants.PIF_PORT_UDP));
					//tcpMasterPort = Integer.parseInt(getOption(PIFComConstants.PIF_PORT_TCP));
					return null;
				}
				hostAdr = InetAddress.getLocalHost();
				//setExecutable(getOption(PrologInterface.PREF_EXECUTABLE));
				if (getExecutable() == null) {
					throw new RuntimeException("option not set: " + PrologInterface.PREF_EXECUTABLE);
				}

				//setFileSearchPath(getOption(PrologInterface.PREF_FILE_SEARCH_PATH));

				//setEnvironment(pif.getOption(PrologInterface.PREF_ENVIRONMENT));
				if (getEnvironment() == null) {
					throw new RuntimeException("option not set: " + PrologInterface.PREF_ENVIRONMENT);
				}

				//TODO: ggf. check in abstractPIF einbauen
				//String timeoutString = pif.getOption(PrologInterface.PREF_TIMEOUT);
//				if (timeoutString == null) {
//					throw new RuntimeException("option not set: " + PrologInterface.PREF_TIMEOUT);
//				}
//				setTimeout(Integer.parseInt(timeoutString));
				int timeout = getTimeout();
				udpSocket.setSoTimeout(timeout);
				Map env = new HashMap();
				// if(Util.isJava5()){
				// env.putAll(System.getenv());
				// }
				//		
				String[] envarray = Util.split(getEnvironment(), ",");
				for (int i = 0; i < envarray.length; i++) {
					String[] mapping = Util.split(envarray[i], "=");
					env.put(mapping[0], mapping[1]);
				}
				envarray = new String[env.size()];
				int i = 0;
				for (Iterator it = env.keySet().iterator(); it.hasNext();) {
					String key = (String) it.next();
					String value = (String) env.get(key);
					envarray[i++] = key + "=" + value;
				}

				int udpLocalPort = udpSocket.getLocalPort();
				String[] command = Util.split(getExecutable(), " ");
				String[] args;
				if (getFileSearchPath() == null || getFileSearchPath().trim().length() == 0) {
					args = new String[] {

					"-g", "[library('pifcom_server')],pifcom_start_server(" + udpLocalPort + ")", };
				} else {
					String fsp = getFileSearchPath();
					//fsp = "library=Y:/workspaces/workspace_dev_pdt_jt_gvf/pdt.runtime.pifcom/library/pifcom";
				
					args = new String[] { "-p", fsp, "-g", "[library('pifcom_server')],pifcom_start_server(" + udpLocalPort + ")", };
				}
				;
				String[] commandArray = new String[command.length + args.length];
				System.arraycopy(command, 0, commandArray, 0, command.length);
				System.arraycopy(args, 0, commandArray, command.length, args.length);

				// start server
				Debug.info("Starting server with " + Util.prettyPrint(commandArray));
				if (envarray.length == 0) {
					Debug.info("inheriting system environment");
					process = Runtime.getRuntime().exec(commandArray);
				} else {
					Debug.info("using environment: " + Util.prettyPrint(envarray));
					process = Runtime.getRuntime().exec(commandArray, envarray);
				}
				new _Pump(process.getErrorStream()).start();
				new _Pump(process.getInputStream()).start();

				// wait for handshake
				byte[] buf = new byte[4];
				DatagramPacket p = new DatagramPacket(buf, 4);

				udpSocket.receive(p);

				setTcpMasterPort(((buf[0] & 0xFF) << 8) + (buf[1] & 0xFF));
				setUdpMasterPort(((buf[2] & 0xFF) << 8) + (buf[3] & 0xFF));
				return process;
			}

			public void stopServer(PrologInterface pif) throws Throwable {
				if (!isRunning(pif)) {
					Debug.warning("Server process does not appear to running, so I will not stop it.");
					return;
				}
				DatagramPacket p = new DatagramPacket("die!".getBytes(), 4);
				p.setAddress(InetAddress.getLocalHost());
				p.setPort(getUdpMasterPort());
				udpSocket.send(p);
				process.waitFor();
				udpSocket.close();

			}

		};
	}



	private final class _Pump extends InputStreamPump {
		private _Pump(InputStream s) {
			super(s);
		}

		protected void dataAvailable(char[] buffer, int length) {
			System.err.print(new String(buffer, 0, length));
			System.err.flush();
		}
	}
	
	 // =============================================================
	 // modified from factory
	 // =============================================================
   
	public static PrologInterface newInstance(){
	    	
	        return newInstance("egal",null);
	    }
	    
	    public static PrologInterface newInstance(String fqn,String name) {
	    	// fqn ist egal, da diese Methode die allgemeinere aus der abstrakten oberklasse überschreibt

	    	return new PIFComPrologInterface(name);
	    }

}
