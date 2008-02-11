package org.cs3.pifcom.codec;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;

import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.pifcom.Factory;

import junit.framework.TestCase;

public class PifcomTest extends TestCase {

	private final class _Pump extends InputStreamPump {
		private _Pump(InputStream s) {
			super(s);
		}

		@Override
		protected void dataAvailable(char[] buffer, int length) {
			System.err.print(new String(buffer, 0, length));
			System.err.flush();
		}
	}

	private Process process;

	private InputStreamPump errPump;

	private DataInputStream in;

	private DataOutputStream out;

	private boolean isSetup = false;

	private _Pump outPump;

	private DatagramSocket udpSocket;

	private int tcpMasterPort;

	private int udpMasterPort;

	private int udpLocalPort;

	private int udpSlavePort;

	protected void setUp() throws IOException {
		String executable = System.getProperty("pif.executable", Factory
				.guessExecutableName());

		String path = System.getProperty("prolog.library_path");
		if (path == null) {
			throw new RuntimeException(
					"Please set the system property prolog.library_path."
							+ "The value is passed to "
							+ "SWI-Prolog using the -p command line option.");
		}

		udpSocket = new DatagramSocket(new InetSocketAddress(0));
		udpLocalPort = udpSocket.getLocalPort();
		String[] command = Util.split(executable, " ");
		String[] args = new String[] {
				"-p",
				path,
				"-g",
				"[library('pifcom/pifcom_server')],pifcom_run_server("
						+ udpLocalPort + ")", };
		String[] commandArray = new String[command.length + args.length];
		System.arraycopy(command, 0, commandArray, 0, command.length);
		System.arraycopy(args, 0, commandArray, command.length, args.length);

		// start server
		this.process = Runtime.getRuntime().exec(commandArray);
		this.errPump = new _Pump(process.getErrorStream());
		this.outPump = new _Pump(process.getInputStream());
		errPump.start();
		outPump.start();

		// wait for handshake
		byte[] buf = new byte[4];
		DatagramPacket p = new DatagramPacket(buf, 4);
		udpSocket.receive(p);

		tcpMasterPort = ((buf[0] & 0xFF) << 8) + (buf[1] & 0xFF);
		udpMasterPort = ((buf[2] & 0xFF) << 8) + (buf[3] & 0xFF);
		// connect
		Socket s = new Socket((String) null, tcpMasterPort);
		in = new DataInputStream(new BufferedInputStream(s.getInputStream()));
		out = new DataOutputStream(
				new BufferedOutputStream(s.getOutputStream()));

		udpSlavePort = in.readUnsignedShort();
		isSetup = true;
	}

	@Override
	protected void tearDown() throws Exception {
		if (isSetup) {
			in.close();
			out.close();
			process.waitFor();
			errPump.join();
			outPump.join();
			udpSocket.close();
		}
	}

	public void testEcho() throws Throwable {
		Message m = new Message(Message.OPC_MARK, 42, new byte[0]);
		m.write(out);
		out.flush();
		Message m2 = Message.read(in);
		assertEquals(m.getTicket(), m2.getTicket());
	}

}
