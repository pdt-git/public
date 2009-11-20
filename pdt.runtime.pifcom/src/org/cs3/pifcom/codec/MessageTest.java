
package org.cs3.pifcom.codec;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Socket;

import junit.framework.TestCase;

import org.cs3.pdt.runtime.preferences.PreferenceInitializer;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;

public class MessageTest extends TestCase {
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

	private boolean isSetup=false;

	private _Pump outPump;;

	@Override
	protected void setUp() throws Exception {
		
	}

	
	
	private void setUp_echo() throws IOException {
		String executable = System.getProperty("executable", PreferenceInitializer.guessExecutableName());

		String prolog_main = System.getProperty("prolog_main");
		if (prolog_main == null) {
			throw new RuntimeException(
					"Please set the system property prolog_main.");
		}

		int port = Util.findFreePort();
		String[] command = Util.split(executable, " ");
		String[] args = new String[] { "-g", "['" + prolog_main + "'],pifcom_echo_raw("+port+")",

		};
		String[] commandArray = new String[command.length + args.length];
		System.arraycopy(command, 0, commandArray, 0, command.length);
		System.arraycopy(args, 0, commandArray, command.length, args.length);

		
		//open udp socket to receive handshake
		DatagramSocket ds = new DatagramSocket(port);
		//start server
		this.process = Runtime.getRuntime().exec(commandArray);
		this.errPump = new _Pump(process.getErrorStream());
		this.outPump = new _Pump(process.getInputStream());
			
		//wait for handshake
		byte[] buf = new byte[255];
		DatagramPacket p = new DatagramPacket(buf,255);		
		ds.receive(p);
		
		ds.close();
		
		//connect
		Socket s = new Socket((String)null,port);
		in = new DataInputStream(new BufferedInputStream(
						s.getInputStream()));
		out = new DataOutputStream(new BufferedOutputStream(
						s.getOutputStream()));
		errPump.start();
		outPump.start();
		isSetup=true;
	}

	

	
	
	@Override
	protected void tearDown() throws Exception {
		if(isSetup){
		in.close();
		out.close();
		process.waitFor();
		errPump.join();
		outPump.join();
		
		}
	}



public void test_file_tiny() throws Throwable{
	File file = File.createTempFile("doof", ".raw");
	
	
	
	byte[] body = {0,0x7F,-0x80,-0x01};
	int N = body.length;
	Message m = new Message(Message.OPC_BINDING,42,body);
	out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)));
	m.write(out);
	out.flush();
	in = new DataInputStream(new BufferedInputStream(new FileInputStream(file)));
	Message m2 =Message.read(in);
	//compare byte by byte.
	for(int i=0;i<N;i++){
		assertEquals(body[i], m2.getBody()[i]);			
	}
}
	
public void test_ubyte_file() throws Throwable {
	
	
	byte[] data = {0,0x7F,-0x80,-0x01};
	int N=data.length;
	File file = File.createTempFile("doof", ".raw");
	FileOutputStream ostream = new FileOutputStream(file);
	ostream.write(data);
	ostream.close();
	FileInputStream istream = new FileInputStream(file);
	byte[] data2 = new byte[N];
	assertEquals(N,istream.read(data2));
	for(int i=0;i<N;i++){
		assertEquals(data[i], data2[i]);			
	}
}
public void test_echo_raw_tiny() throws Throwable {
	setUp_echo();	
		
		
		//create random data fitting into 3 fragments
		int N = 5;
		byte[] body = new byte[N];
		for(int i=0;i<N;i++){
			body[i]=(byte) ((Math.random()* 0xFF) -0x80);			
		}
		Message m = new Message(Message.OPC_BINDING,42,body);
		m.write(out);
		out.flush();
		Message m2 =Message.read(in);
		//compare byte by byte.
		for(int i=0;i<N;i++){
			assertEquals(body[i], m2.getBody()[i]);			
		}
	}
	
	public void test_echo_raw_large_aligned() throws Throwable {
		setUp_echo();
		
		int N = 3* 0xFFFF;
		byte[] body = new byte[N];
		for(int i=0;i<N;i++){
			body[i]=(byte) ((Math.random()* 0xFF) -0x80);			
		}
		Message m = new Message(Message.OPC_BINDING,42,body);
		m.write(out);
		out.flush();
		Message m2 =Message.read(in);
		//compare byte by byte.
		for(int i=0;i<N;i++){
			assertEquals(body[i], m2.getBody()[i]);			
		}
	}

	public void test_echo_raw_large_unaligned() throws Throwable {
		setUp_echo();
		
		//create random data fitting into 3 fragments
		int N = 3* 0xF000;
		byte[] body = new byte[N];
		for(int i=0;i<N;i++){
			body[i]=(byte) ((Math.random()* 0xFF) -0x80);			
		}
		Message m = new Message(Message.OPC_BINDING,42,body);
		m.write(out);
		out.flush();
		Message m2 =Message.read(in);
		//compare byte by byte.
		for(int i=0;i<N;i++){
			assertEquals(body[i], m2.getBody()[i]);			
		}
	}


	public void test_echo_raw_small() throws Throwable {
		setUp_echo();
		
		//create random data fitting into 3 fragments
		int N = 0xF00;
		byte[] body = new byte[N];
		for(int i=0;i<N;i++){
			body[i]=(byte) ((Math.random()* 0xFF) -0x80);			
		}
		Message m = new Message(Message.OPC_BINDING,42,body);
		m.write(out);
		out.flush();
		Message m2 =Message.read(in);
		//compare byte by byte.
		for(int i=0;i<N;i++){
			assertEquals(body[i], m2.getBody()[i]);			
		}
	}
	public void test_echo_raw_small_alligned() throws Throwable {
		setUp_echo();
		
		//create random data fitting into 3 fragments
		int N = 0xFFFF;
		byte[] body = new byte[N];
		for(int i=0;i<N;i++){
			body[i]=(byte) ((Math.random()* 0xFF) -0x80);			
		}
		Message m = new Message(Message.OPC_BINDING,42,body);
		m.write(out);
		out.flush();
		Message m2 =Message.read(in);
		//compare byte by byte.
		for(int i=0;i<N;i++){
			assertEquals(body[i], m2.getBody()[i]);			
		}
	}

	
}
