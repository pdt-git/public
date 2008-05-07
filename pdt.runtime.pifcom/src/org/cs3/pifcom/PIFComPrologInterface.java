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

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;
import org.cs3.pl.prolog.internal.AbstractPrologInterface2;
import org.cs3.pl.prolog.internal.ReusablePool;

public class PIFComPrologInterface extends AbstractPrologInterface2 {

	DatagramSocket udpSocket = null;
	private ReusablePool pool = null;

	private InetAddress host;
	private PrologInterfaceFactory factory;
	private ServerStartAndStopStrategy startAndStopStrategy;
	protected Process process;
	protected int tcpMasterPort;
	protected int udpMasterPort;

	private HashMap<String, String> preferences = new HashMap();

	
	public void setOption(String opt, String value) {
		Option[] options = factory.getOptions();
		for (Option option : options) {
			if (option.getId().equals(opt)) {
				this.preferences.put(opt, value);
				return;
			}
		}
		super.setOption(opt, value);
	}

	
	public String getOption(String opt) {
		Option[] options = factory.getOptions();
		for (Option option : options) {
			if (option.getId().equals(opt)) {
				return this.preferences.get(opt);
			}
		}

		return super.getOption(opt);
	}

	public PIFComPrologInterface(Factory factory, String name) {
		super(name);
		this.factory = factory;

	}
	public PIFComPrologInterface(Factory factory) {
	
		this.factory = factory;

	}

	
	public AsyncPrologSession getAsyncSession_impl() throws Throwable {
		ReusablePIFComConnection connection = getConnection();
		return new AsynchronousPIFComSession(connection,this);
	}

	private ReusablePIFComConnection getConnection()
			throws UnknownHostException, IOException {
		ReusablePIFComConnection connection = null;
		if (pool != null) {
			connection = (ReusablePIFComConnection) pool
					.findInstance(ReusablePIFComConnection.class);
		}
		if (connection == null) {
			connection = new ReusablePIFComConnection(udpSocket, host,
					tcpMasterPort, pool) {

				
				protected void error(Throwable e)
						throws PrologInterfaceException {
					PIFComPrologInterface.this.error(e);

				}

			};
		}
		return connection;
	}

	
	public PrologSession getSession_impl() throws Throwable {
		ReusablePIFComConnection connection = getConnection();
		return new PIFComSession(connection,this);
	}

	public PrologInterfaceFactory getFactory() {

		return factory;
	}

	
	public ServerStartAndStopStrategy getStartAndStopStrategy() {

		return new ServerStartAndStopStrategy() {

			
			public boolean isRunning(PrologInterface pif) {
				if(process==null){
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
				if( isRunning(pif)){
					Debug.warning("Server process appears to be already running, so I will not start it.");
					return process;
				}
				udpSocket = new DatagramSocket(new InetSocketAddress(0));
				if (Boolean.valueOf(getOption("pif.standalone")).booleanValue()) {
					Debug
							.warning("Will not start server; the option pif.standalone is set.");
					host = InetAddress.getByName(getOption("pif.host"));

					udpMasterPort = Integer.parseInt(getOption("pif.udp_port"));
					tcpMasterPort = Integer.parseInt(getOption("pif.tcp_port"));
					return null;
				}
				host = InetAddress.getLocalHost();
				String executable = getOption("prolog.executable");
				if (executable == null) {
					throw new RuntimeException(
							"option not set: prolog.executable");
				}

				String path = getOption(PrologInterface.FILE_SEARCH_PATH);
				

				String envstring = pif.getOption("prolog.environment");
				if (envstring == null) {
					throw new RuntimeException(
							"option not set: prolog.environment");
				}
				
				String timeoutString = pif.getOption("pif.timeout");
				if (timeoutString == null) {
					throw new RuntimeException(
							"option not set: pif.timeout");
				}
				int timeout = Integer.parseInt(timeoutString);
				udpSocket.setSoTimeout(timeout);
				Map env = new HashMap();
				// if(Util.isJava5()){
				// env.putAll(System.getenv());
				// }
				//		
				String[] envarray = Util.split(envstring, ",");
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
				String[] command = Util.split(executable, " ");
				String[] args;
				if(path==null||path.trim().length() == 0){
					args = new String[] {
							
							"-g",
							"[library('pifcom_server')],pifcom_start_server("
									+ udpLocalPort + ")", };
				}
				else{
				args = new String[] {
						"-p",
						path,
						"-g",
						"[library('pifcom_server')],pifcom_start_server("
								+ udpLocalPort + ")", };
				};
				String[] commandArray = new String[command.length + args.length];
				System.arraycopy(command, 0, commandArray, 0, command.length);
				System.arraycopy(args, 0, commandArray, command.length,
						args.length);

				// start server
				Debug.info("Starting server with "
						+ Util.prettyPrint(commandArray));
				if (envarray.length == 0) {
					Debug.info("inheriting system environment");
					process = Runtime.getRuntime().exec(commandArray);
				} else {
					Debug.info("using environment: "
							+ Util.prettyPrint(envarray));
					process = Runtime.getRuntime().exec(commandArray, envarray);
				}
				new _Pump(process.getErrorStream()).start();
				new _Pump(process.getInputStream()).start();

				// wait for handshake
				byte[] buf = new byte[4];
				DatagramPacket p = new DatagramPacket(buf, 4);
				
				udpSocket.receive(p);

				tcpMasterPort = ((buf[0] & 0xFF) << 8) + (buf[1] & 0xFF);
				udpMasterPort = ((buf[2] & 0xFF) << 8) + (buf[3] & 0xFF);
				return process;
			}

			
			public void stopServer(PrologInterface pif) throws Throwable {
				if(! isRunning(pif)){
					Debug.warning("Server process does not appear to running, so I will not stop it.");
					return;
				}
				DatagramPacket p = new DatagramPacket("die!".getBytes(), 4);
				p.setAddress(InetAddress.getLocalHost());
				p.setPort(udpMasterPort);
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

}
