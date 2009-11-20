package org.cs3.pifcom;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;

import junit.framework.TestCase;

import org.cs3.pdt.runtime.preferences.PreferenceInitializer;
import org.cs3.pifcom.codec.CTermMessage;
import org.cs3.pifcom.codec.Message;
import org.cs3.pifcom.codec.NameMessage;
import org.cs3.pifcom.codec.UIntMessage;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PrologInterface;

/*
 * Starts a prolog process running the pifcom_server.
 * Runs several scenarios to see whether the server conforms to the 
 * protocol specification. 
 */
public class PIFComServerTest extends TestCase {

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

	private String processorThreadAlias;

	protected void setUp() throws IOException {
		String executable = System.getProperty(PrologInterface.PREF_EXECUTABLE, 
				PreferenceInitializer.guessExecutableName());

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
						+ udpLocalPort + "),halt", };
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
		NameMessage m = (NameMessage) Message.read(in);
		processorThreadAlias = m.getStringValue();
		isSetup = true;
	}

	@Override
	protected void tearDown() throws Exception {
		DatagramPacket p = new DatagramPacket("die!".getBytes(),4);
		p.setAddress(InetAddress.getLocalHost());
		p.setPort(udpMasterPort);
		udpSocket.send(p);
		if (isSetup) {
			in.close();
			out.close();
			process.waitFor();
			errPump.join();
			outPump.join();
			udpSocket.close();
		}
	}

	/**
	 * sends a MARK message and expects it to be echoed by the
	 * server.
	 * @throws Throwable
	 */
	public void testEcho() throws Throwable {
		Message m = Message.mark(42);
		m.write(out);
		out.flush();
		Message m2 = Message.read(in);
		assertEquals(m.getTicket(), m2.getTicket());
		Message m3 = Message.bye(23);
		m3.write(out);
		out.flush();
		Message m4 = Message.read(in);
		assertEquals(Message.OPC_BYE,m4.getOpCode());
		in.close();
		out.close();		
	}
	
	/**
	 * sends a QUERY message,
	 * expects three solutions, each binding one variable.
	 * @throws Throwable
	 */
	public void testSimpleQuery() throws Throwable{
		Message.query(42, "member(A,[1,2,3]).").write(out);
		Message.bye(23).write(out);
		out.flush();
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(1,((UIntMessage)m).getIntValue());
		
		m = Message.read(in);
		assertTrue(m instanceof NameMessage);
		assertEquals(Message.OPC_NAME,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("A",((NameMessage)m).getStringValue());
		
		
		m = Message.read(in);
		assertTrue(m instanceof CTermMessage);
		assertEquals(Message.OPC_BINDING,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("1",((CTermMessage)m).getStringValue());
		
		
		m = Message.read(in);
		assertTrue(m instanceof CTermMessage);
		assertEquals(Message.OPC_BINDING,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("2",((CTermMessage)m).getStringValue());
		
		
		m = Message.read(in);
		assertTrue(m instanceof CTermMessage);
		assertEquals(Message.OPC_BINDING,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("3",((CTermMessage)m).getStringValue());
		
		assertEquals(Message.OPC_FAIL,Message.read(in).getOpCode());
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
	}
	
	/**
	 * sends a QUERY message,
	 * expects three empty solutions. 
	 * @throws Throwable
	 */	
	public void testSimpleQuery_noBindings() throws Throwable{
		Message.query(42, "member(1,[1,1,1]).").write(out);
		Message.bye(23).write(out);
		out.flush();
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(0,((UIntMessage)m).getIntValue());
		assertEquals(Message.OPC_EMPTY,Message.read(in).getOpCode());
		assertEquals(Message.OPC_EMPTY,Message.read(in).getOpCode());
		assertEquals(Message.OPC_EMPTY,Message.read(in).getOpCode());
		assertEquals(Message.OPC_FAIL,Message.read(in).getOpCode());
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
	}
	
	/**
	 * sends a QUERY message,
	 * expects no solutions and no bindings. 
	 * @throws Throwable
	 */
	public void testSimpleQuery_noSolutionNoBindings() throws Throwable{
		Message.query(42, "member(1,[]).").write(out);
		Message.bye(23).write(out);
		out.flush();
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(0,((UIntMessage)m).getIntValue());
		assertEquals(Message.OPC_FAIL,Message.read(in).getOpCode());
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
	}
	
	/**
	 * sends a QUERY message,
	 * expects no solutions but two name bindings. 
	 * @throws Throwable
	 */
	public void testSimpleQuery_noSolutionTwoBindings() throws Throwable{
		Message.query(42, "member((A,B),[]).").write(out);
		Message.bye(23).write(out);
		out.flush();
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(2,((UIntMessage)m).getIntValue());
		
		m = Message.read(in);
		assertTrue(m instanceof NameMessage);
		assertEquals(Message.OPC_NAME,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("A",((NameMessage)m).getStringValue());
		m = Message.read(in);
		assertTrue(m instanceof NameMessage);
		assertEquals(Message.OPC_NAME,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("B",((NameMessage)m).getStringValue());
		
		assertEquals(Message.OPC_FAIL,Message.read(in).getOpCode());
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
	}

	/**
	 * sends a QUERY and an ABORT message on batch.
	 * wait some time, sends an ABORT message on control.
	 * The Query produces infinite solutions each binding a single variable, 
	 * but is expected to be cut by the abort.
	 * expects several solutions each binding one variable.
	 * then one CUT and finally a COMPLETE. 
	 * @throws Throwable
	 */
	public void testAbort_cut() throws Throwable{
		Message.query(42, "repeat,flag(testflag,A,A+1),(A>10->!;sleep(0.5)).").write(out);
		Message.abort(43).write(out);
		Message.bye(23).write(out);
		out.flush();
		Thread.sleep(1000);
		Message.abort(43).send(udpSocket, InetAddress.getLocalHost(), udpSlavePort);
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(1,((UIntMessage)m).getIntValue());
		
		m = Message.read(in);
		assertTrue(m instanceof NameMessage);
		assertEquals(Message.OPC_NAME,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("A",((NameMessage)m).getStringValue());
		int i=0;
		while((m=Message.read(in)).getOpCode()==Message.OPC_BINDING){
			assertTrue(m instanceof CTermMessage);
			assertEquals(Message.OPC_BINDING,m.getOpCode());
			assertEquals(42,m.getTicket());
			assertEquals(""+(i++),((CTermMessage)m).getStringValue());
		}
		assertEquals(Message.OPC_CUT,m.getOpCode());
		assertEquals(42,m.getTicket());
		m= Message.read(in);
		assertEquals(Message.OPC_COMPLETE,m.getOpCode());
		assertEquals(43,m.getTicket());	
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
	}
	
	/**
	 * sends three QUERY and one ABORT message on batch.
	 * wait some time, sends an ABORT message on control.
	 * The first query produces infinite solutions each binding a single variable, 
	 * but is expected to be cut by the abort.
	 * expects several solutions to the first query, each binding one variable.
	 * then one CUT and two SKIP Messages,
	 * finally a COMPLETE. 
	 * @throws Throwable
	 */
	public void testAbort_skip() throws Throwable{
		Message.query(42, "repeat,flag(testflag,A,A+1),(A>10->!;sleep(0.5)).").write(out);
		Message.query(96, "true.").write(out);
		Message.query(97, "true.").write(out);
		Message.abort(43).write(out);
		Message.bye(23).write(out);
		out.flush();
		Thread.sleep(1000);
		Message.abort(43).send(udpSocket, InetAddress.getLocalHost(), udpSlavePort);
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(1,((UIntMessage)m).getIntValue());
		
		m = Message.read(in);
		assertTrue(m instanceof NameMessage);
		assertEquals(Message.OPC_NAME,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals("A",((NameMessage)m).getStringValue());
		int i=0;
		while((m=Message.read(in)).getOpCode()==Message.OPC_BINDING){
			assertTrue(m instanceof CTermMessage);
			assertEquals(Message.OPC_BINDING,m.getOpCode());
			assertEquals(42,m.getTicket());
			assertEquals(""+(i++),((CTermMessage)m).getStringValue());
		}
		assertEquals(Message.OPC_CUT,m.getOpCode());
		assertEquals(42,m.getTicket());
		m= Message.read(in);
		assertEquals(Message.OPC_SKIP,m.getOpCode());
		assertEquals(96,m.getTicket());
		m= Message.read(in);
		assertEquals(Message.OPC_SKIP,m.getOpCode());
		assertEquals(97,m.getTicket());
		m= Message.read(in);
		assertEquals(Message.OPC_COMPLETE,m.getOpCode());
		assertEquals(43,m.getTicket());	
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
	}

	public void testSyntaxError() throws Throwable{
		Message.query(42, "syntax error.").write(out);
		Message.bye(23).write(out);
		out.flush();
		Message m= Message.read(in);
		assertEquals(Message.OPC_PROTOCOL_ERROR,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertTrue(m instanceof CTermMessage);
		// expect an exception like the following:
		//error(syntax_error(operator_expected), stream(user_input, 4, 0, 131))
		CTerm term = ((CTermMessage)m).getCTermValue();
		assertEquals("error",term.getFunctorValue());
		assertEquals(2,term.getArity());
		term = ((CCompound)term).getArgument(0);
		assertEquals("syntax_error",term.getFunctorValue());
		assertEquals(1,term.getArity());
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
		
	}
	
	public void testPrologError() throws Throwable{
		Message.query(42, "throw(my_error).").write(out);
		Message.bye(23).write(out);
		out.flush();
		
		Message m = Message.read(in);
		assertTrue(m instanceof UIntMessage);
		assertEquals(Message.OPC_BEGIN_SOLUTION,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertEquals(0,((UIntMessage)m).getIntValue());
		
		m= Message.read(in);
		assertEquals(Message.OPC_ERROR,m.getOpCode());
		assertEquals(42,m.getTicket());
		assertTrue(m instanceof CTermMessage);
		CTerm term = ((CTermMessage)m).getCTermValue();
		assertEquals("my_error",term.getFunctorValue());
		assertEquals(0,term.getArity());
		
		assertEquals(Message.OPC_BYE,Message.read(in).getOpCode());
		
	}
}
