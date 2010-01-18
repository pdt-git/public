/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.prolog.internal.socket;

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

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.internal.ReusablePool;


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
				pool.recycle(socket);
			} else {
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
