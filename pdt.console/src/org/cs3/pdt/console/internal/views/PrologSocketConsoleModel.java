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

package org.cs3.pdt.console.internal.views;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.ConsoleModelEvent;
import org.cs3.pl.console.ConsoleModelListener;
import org.cs3.pl.console.prolog.QueryExpansion;

/**
 * @author terra
 */
public class PrologSocketConsoleModel implements ConsoleModel {

	private int port;

	private Thread readerThread = null;

	private boolean singleCharMode = false;

	private String lineBuffer = "";

	private BufferedWriter writer;

	private HashSet<ConsoleModelListener> listeners = new HashSet<ConsoleModelListener>();

	private Socket socket;

	private Vector<QueryExpansion> expansions = new Vector<QueryExpansion>();

	private boolean disconnecting;


	private class ConsoleReader implements Runnable {

		private static final int ESCAPE_CHAR = '*';

		private static final char RAW_MODE_CHAR = 's';

		private static final char COOKED_MODE_CHAR = 'c';

		private static final char NO_MODE_CHAR = 'n';

		private BufferedReader reader;

		public ConsoleReader(BufferedReader reader) {
			this.reader = reader;
		}

		@Override
		public void run() {
			char[] buf = new char[255];
			try {
				int count = reader.read(buf);
				StringBuffer data = new StringBuffer();

				while (count > 0) {

					boolean escaped = false;
					for (int i = 0; i < count; i++) {
						if (escaped) {
							switch (buf[i]) {
							case RAW_MODE_CHAR:
								// report what we have until mode switch:
								ConsoleModelEvent cme = new ConsoleModelEvent(
										PrologSocketConsoleModel.this, data
												.toString(), false);
								fireOutputEvent(cme);
								// clear output buffer
								data.setLength(0);

								setSingleCharMode(true);
								break;
							case COOKED_MODE_CHAR:
							case NO_MODE_CHAR:
								// disable single char mode again. should not be
								// neccessary...
								setSingleCharMode(false);
								break;
							case ESCAPE_CHAR:
								data.append(buf[i]);
								break;
							default:
								// current default is to echo the escaped char.
								data.append(buf[i]);
								break;
							}

							// our "protocol" allows an optional newline after
							// the escaped character.
							// If its there, we skip it silently.
							if (i + 1 < count && buf[i + 1] == '\n') {
								i++;
							}

							escaped = false;
						} else {
							if (buf[i] == ESCAPE_CHAR) {
								escaped = true;
							} else {
								data.append(buf[i]);
							}
						}
					}

					if (!reader.ready() || data.length() >= 500) {
						
						if (data.length() > 0) {
							ConsoleModelEvent cme = new ConsoleModelEvent(
									PrologSocketConsoleModel.this, data
											.toString(), false);
							fireOutputEvent(cme);
							data.setLength(0);
						}
					} 
					count = reader.read(buf);

				}

			} catch (Throwable e) {
				Debug.report(e);

			}
			disconnect();
			Debug.info("Console Reader Thread terminating");
		}

	}

	public PrologSocketConsoleModel() {
		this.port = 5567;
		connect();
	}

	public PrologSocketConsoleModel(boolean doConnect) {
		this.port = 5567;
		if (doConnect) {
			connect();
		}
	}

	@Override
	public String getLineBuffer() {
		return lineBuffer;
	}

	@Override
	public void setLineBuffer(String buffer) {
		String oldBuffer = lineBuffer;
		lineBuffer = buffer;
		fireEditBufferChangedEvent(oldBuffer, lineBuffer);
	}

	@Override
	synchronized public void commitLineBuffer() {
		if (singleCharMode)
			throw new IllegalStateException("In single char mode");

		if (!isConnected()) {
			connect();
		}

		ConsoleModelEvent cme = new ConsoleModelEvent(
				PrologSocketConsoleModel.this, lineBuffer + "\n", false);

		fireOutputEvent(cme);

		try {
			writer.write(expandQuery(lineBuffer));
			writer.write("\n");
			writer.flush();

			cme = new ConsoleModelEvent(this, lineBuffer);

			fireCommitEvent(cme);

			// clear linebuffer.
			String oldBuffer = lineBuffer;
			lineBuffer = "";
			fireEditBufferChangedEvent(oldBuffer, lineBuffer);
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	private String expandQuery(String query) {
		for (Iterator<QueryExpansion> it = expansions.iterator(); it.hasNext();) {
			QueryExpansion exp = it.next();
			query = exp.apply(query);
		}
		return query;
	}

	public void registerQueryExpansion(QueryExpansion s) {
		synchronized (expansions) {
			if (!expansions.contains(s)) {
				expansions.add(s);
			}
		}
	}

	public void unregisterQueryExpansion(QueryExpansion s) {
		synchronized (expansions) {
			if (expansions.contains(s)) {
				expansions.remove(s);
			}
		}
	}

	@Override
	public void addConsoleListener(ConsoleModelListener cml) {
		synchronized (listeners) {
			listeners.add(cml);
		}
	}

	@Override
	public void removeConsoleListener(ConsoleModelListener cml) {
		synchronized (listeners) {
			listeners.remove(cml);
		}
	}

	/**
	 * precondition: we are in single char mode postcondition: we are in line
	 * mode.
	 */
	@Override
	public void putSingleChar(char c) {
		if (!singleCharMode) {
			throw new IllegalStateException("In line mode");
		}

		synchronized (this) {
			if (!isConnected()) {
				try {
					wait(500);
					connect();
				} catch (InterruptedException e1) {
					return;
				}
			}

			try {

				/*
				 * the newline is required. prolog side does not flush buffers
				 * otherwise. we will skip it away on the prolog side.
				 */
				writer.write(c + "\n");
				writer.flush();
				/*
				 * switch back to line mode
				 */
				setSingleCharMode(false);
			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
	}

	protected void setSingleCharMode(boolean on) {
		if (on != this.singleCharMode) {
			this.singleCharMode = on;
			fireModeChange(new ConsoleModelEvent(this, on));
		}
	}

	@Override
	public boolean isSingleCharMode() {
		return singleCharMode;
	}

	@Override
	public synchronized void connect()  {
		if (isConnected()) {
			Debug.warning("Seems we are already connected?");
			return;
		}
		try {
			Debug.info("connecting console to server at port " + port);
			socket = new Socket((String) null, port);
			writer = new BufferedWriter(new OutputStreamWriter(socket
					.getOutputStream()));
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					socket.getInputStream()));
			readerThread = new Thread(new ConsoleReader(reader));
			readerThread.setDaemon(true);
			readerThread.setName("Console Reader Thread");
			readerThread.start();

			String valString = PrologConsolePlugin.getDefault()
					.getPreferenceValue(PDTConsole.PREF_INTERCEPT_GET_SINGLE_CHAR,
							"false");
			boolean useVoodoo = Boolean.valueOf(valString).booleanValue();
			if (useVoodoo) {
				writer.write("use_module(lib_pdt_console_pl('cio/single_char_interceptor.pl')).\n"
//						+ "sci_install.\n"
						+ "set_stream(current_output,tty(true)),"
						+ "set_stream(current_input,tty(true)).\n");
			} 
			writer.flush();
			Debug.debug("Connect complete");
			fireAfterConnect();
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		super.finalize();
		disconnect();
	}

	@Override
	public void disconnect() {
		if (disconnecting||!isConnected()) {
			return;
		}
		synchronized (this) {

			disconnecting = true;
			Debug.debug("Disconnect began");
			fireBeforeDisconnect();

			try {
				if (socket != null) {

					writer.write("end_of_file.\n");
					writer.flush();
					if(Thread.currentThread()!=readerThread){
						readerThread.join(10000);
					}
					socket.close();
				}
			} catch (IOException e) {
				Debug.report(e);
			} catch (InterruptedException e) {
				Debug.report(e);
			}

			socket = null;
			writer = null;
			readerThread = null;

			ConsoleModelEvent cme = new ConsoleModelEvent(
					this,
					"<<< Connection to Prolog Process closed >>>\n\n",
					true);

			fireOutputEvent(cme);

			Debug.debug("Disconnect complete");
			disconnecting = false;
		}
	}

	private void fireAfterConnect() {
		ConsoleModelEvent e = new ConsoleModelEvent(this);
		HashSet<ConsoleModelListener> l = getAListenersClone();

		for (Iterator<ConsoleModelListener> i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = i.next();
			list.afterConnect(e);
		}
	}

	private void fireBeforeDisconnect() {
		ConsoleModelEvent e = new ConsoleModelEvent(this);
		HashSet<ConsoleModelListener> l = getAListenersClone();

		for (Iterator<ConsoleModelListener> i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = i.next();
			list.beforeDisconnect(e);
		}
	}

	void fireOutputEvent(ConsoleModelEvent cme) {
		HashSet<ConsoleModelListener> l = getAListenersClone();

		for (Iterator<ConsoleModelListener> i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = i.next();
			list.onOutput(cme);
		}
	}

	void fireModeChange(ConsoleModelEvent cme) {
		HashSet<ConsoleModelListener> l = getAListenersClone();

		for (Iterator<ConsoleModelListener> i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = i.next();
			list.onModeChange(cme);
		}
	}

	private void fireCommitEvent(ConsoleModelEvent cme) {
		HashSet<ConsoleModelListener> l = getAListenersClone();

		for (Iterator<ConsoleModelListener> i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = i.next();
			list.onCommit(cme);
		}
	}

	private void fireEditBufferChangedEvent(String oldBuffer, String buffer) {
		ConsoleModelEvent ev = new ConsoleModelEvent(this, oldBuffer, buffer);

		HashSet<ConsoleModelListener> l = getAListenersClone();

		for (Iterator<ConsoleModelListener> i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = i.next();
			list.onEditBufferChanged(ev);
		}
	}
	
	@SuppressWarnings("unchecked")
	private HashSet<ConsoleModelListener> getAListenersClone() {
		HashSet<ConsoleModelListener> l;
		synchronized (listeners) {
			l = (HashSet<ConsoleModelListener>) listeners.clone();
		}
		return l;
	}

	@Override
	public boolean isConnected() {
		if (socket == null) {
			return false;
		}
		return socket.isConnected();
	}

	/**
	 * @return Returns the port.
	 */
	public int getPort() {
		return port;
	}

	/**
	 * @param port
	 *            The port to set.
	 */
	public void setPort(int port) {
		this.port = port;
	}

	
}
