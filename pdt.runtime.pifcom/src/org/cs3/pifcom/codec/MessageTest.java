package org.cs3.pifcom.codec;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import junit.framework.TestCase;

import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.pifcom.Factory;

public class MessageTest extends TestCase {
	private Process process;

	private InputStreamPump pump;

	private DataInputStream in;

	private DataOutputStream out;

	private boolean isSetup=false;;

	@Override
	protected void setUp() throws Exception {
		
	}

	
	
	private void setUp_echo() throws IOException {
		String executable = System.getProperty("executable", Factory
				.guessExecutableName());

		String prolog_main = System.getProperty("prolog_main");
		if (prolog_main == null) {
			throw new RuntimeException(
					"Please set the system property prolog_main.");
		}

		String[] command = Util.split(executable, " ");
		String[] args = new String[] { "-g", "['" + prolog_main + "']",

		};
		String[] commandArray = new String[command.length + args.length];
		System.arraycopy(command, 0, commandArray, 0, command.length);
		System.arraycopy(args, 0, commandArray, command.length, args.length);

		this.process = Runtime.getRuntime().exec(commandArray);
		this.pump = new InputStreamPump(process.getErrorStream()) {

			@Override
			protected void dataAvailable(char[] buffer, int length) {
				System.err.print(new String(buffer, 0, length));
				System.err.flush();
			}

		};
		in = new DataInputStream(new BufferedInputStream(
						process.getInputStream()));
		out = new DataOutputStream(new BufferedOutputStream(
						process.getOutputStream()));
		pump.start();
		isSetup=true;
	}

	@Override
	protected void tearDown() throws Exception {
		if(isSetup){
		process.waitFor();
		pump.join();
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
		PrintStream outprinter = new PrintStream(out);
//		outprinter.println("guitracer.");
		outprinter.println("pifcom_echo_raw.");
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
		PrintStream outprinter = new PrintStream(out);
		//outprinter.println("guitracer,spy(spyme).");
		outprinter.println("pifcom_echo_raw.");
		//create random data fitting into exactly 3 fragments
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
		PrintStream outprinter = new PrintStream(out);
//		outprinter.println("guitracer.");
		outprinter.println("pifcom_echo_raw.");
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
		PrintStream outprinter = new PrintStream(out);
//		outprinter.println("guitracer.");
		outprinter.println("pifcom_echo_raw.");
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
		PrintStream outprinter = new PrintStream(out);
//		outprinter.println("guitracer.");
		outprinter.println("pifcom_echo_raw.");
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
