/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.internal.pif.socket;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.UnknownHostException;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologException;


public class SocketClient {
	private int lockCounter = 0;
	private Object ownerLock = new Object();
	private BufferedReader reader;
	private ReusableSocket socket;
	private ReusablePool pool;
	private BufferedWriter writer;
	private boolean paranoid=true;
	private String processorThread;
	private long pid;
	private ValueReader valueReader;	

	public SocketClient(String string, int port) throws UnknownHostException,
			IOException {
		this(new ReusableSocket(string, port));
	}
	
	public SocketClient(ReusableSocket socket) throws IOException {
		valueReader=new ValueReader(this);
		this.socket = socket;
		initializeBuffers();
		reset();
		readUntil(SocketCommunicationConstants.GIVE_COMMAND);
		writeln(SocketCommunicationConstants.PING);

		StringBuffer sb = new StringBuffer();
		String pong = readUntil(SocketCommunicationConstants.PONG,sb);

		Debug.debug("got ping reply:"+pong);
		Debug.debug("after skipping:"+sb.toString());
		String[] strings = Util.split(pong,":");
		this.pid=Long.parseLong(strings[0]);
		this.processorThread=strings[1];
		readUntil(SocketCommunicationConstants.OK);
	}

	private void initializeBuffers() throws UnsupportedEncodingException,
			IOException {
		reader = new BufferedReader(new InputStreamReader(getInputStream(),"UTF-8"));
		PrintWriter printWriter = new PrintWriter(new OutputStreamWriter(getOutputStream(),"UTF-8"));
		writer = new BufferedWriter(printWriter);
	}

	public void close() throws IOException {
		if (socket == null) {
			return;
		}
		try {
			if (pool != null) {
				reset();
	            Debug.info("recycle socket: " + socket.getLocalPort());
				pool.recycle(socket);
			} else {
	            Debug.info("destroy socket: " + socket.getLocalPort());
				socket.destroy();
			}
		} finally {
			socket = null;
		}
	}

	public void reset() throws PrologException, IOException {
		while (reader.ready()) {
			reader.read();
		}
		writeln("");
		readUntil(SocketCommunicationConstants.OK);
	}

	private InputStream getInputStream() throws IOException {
		exceptionIfNoSocketThere();
		return new InputStreamProxy(socket.getInputStream(), socket
				.getLogBuffer(),this);
	}

	public OutputStream getOutputStream() throws IOException {
		exceptionIfNoSocketThere();
		return new OutputStreamProxy(socket.getOutputStream(), socket
				.getLogBuffer(),this);
	}

	public String readln() throws IOException {
		return reader.readLine();
	}

	/**
	 * same as readUntil(prefix,null).
	 * 
	 * @param prefix
	 * @return
	 * @throws PrologException
	 * @throws IOException
	 */
	public String readUntil(String prefix) throws PrologException, IOException {
		return readUntil(prefix, null);
	}

	/**
	 * reads until a line with the given prefix is recieved. This call will
	 * block until line is read that is that starts with the given prefix or the
	 * special prefix <cpde>ERROR</code>, or if <code>EndOfStream</code> is
	 * reached. In the two latter cases, a <code>ConsultException</code> is
	 * raised. In all three cases only complete lines (i.e. terminated with a
	 * newline char) are read.
	 * 
	 * @param prefix
	 *            The awaited prefix
	 * @param data
	 *            all data recieved BEFORE the above prefix is appended to this
	 *            StringBuffer. If this argument is <code>null</code>, .the
	 *            data is silently discarded.
	 * @return the remaining postfix of the line beginning with prefix.
	 * @throws IOException
	 *             you never know...
	 * @throws PrologException
	 *             if a line starting with <code>ERROR</code> or <code>
	 *             EndOfStream</code> is recieved.
	 */
	public String readUntil(String prefix, StringBuffer data)
			throws IOException, PrologException {
		exceptionIfNoSocketThere();
		String string = "";
		while (!string.startsWith(prefix)) {
			if (data != null) {
				data.append(string);
			}
			string = reader.readLine();
			if (string == null) {
				Debug.warning("there was an error. Exceptions will be thrown.");
				File logFile = File.createTempFile("failedSession", "log");

				PrintStream p = new PrintStream(new BufferedOutputStream(
						new FileOutputStream(logFile)));
				socket.getLogBuffer().printLog(p);
				p.close();
				Debug.warning("a connection log was saved to :"
						+ logFile.getCanonicalPath());
				throw new IOException("EndOfStream read while waiting for "
						+ prefix);
			}
			
			if (string.startsWith(SocketCommunicationConstants.ERROR)) {
				throw new PrologException(string.substring(SocketCommunicationConstants.ERROR.length()));
			}
		}
		return string.substring(prefix.length());
	}

	private void exceptionIfNoSocketThere() {
		if (socket == null) {
			throw new IllegalStateException("Socket is closed, go away. ");
		}
	}

	public void setParanoiaEnabled(boolean b) {
		synchronized (ownerLock) {
			if (lockCounter != 0) {
				throw new IllegalThreadStateException(
						"Cannot enable/disable paranoia while client is locked.");
			}
			this.paranoid = b;
		}
	}

	public boolean isParanoiaEnabled() {
		synchronized (ownerLock) {
			return paranoid;
		}
	}

	public void writeln(String line) throws IOException {
		exceptionIfNoSocketThere();
		writer.write(line + SocketCommunicationConstants.LINE_SEPARATOR);
		writer.flush();
	}

	public ReusablePool getPool() {
		return pool;
	}

	public void setPool(ReusablePool pool) {
		this.pool = pool;
	}

	/**
	 * @return Returns the reader.
	 */
	public BufferedReader getReader() {
		return reader;
	}

	public String getProcessorThread() {
		return processorThread;
	}
	public long getServerPid(){
		return pid;
	}
	
	public  Object readValue(int flags) throws IOException {
		return valueReader.readValue(flags);
	}
	
}

