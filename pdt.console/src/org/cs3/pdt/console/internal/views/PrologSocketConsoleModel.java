package org.cs3.pdt.console.internal.views;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
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
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.ConsoleModelEvent;
import org.cs3.pl.console.ConsoleModelListener;
import org.cs3.pl.console.prolog.QueryExpansion;

/**
 * @author terra
 */
public class PrologSocketConsoleModel implements ConsoleModel {

	private class ConsoleReader implements Runnable {

		private static final int ESCAPE_CHAR = 42;

		private static final char RAW_MODE_CHAR = 's';

		private BufferedReader reader;

		public ConsoleReader(BufferedReader reader) {
			this.reader = reader;
		}

		public void run() {
			char[] buf = new char[255];
			try {
				int count = reader.read(buf);
				StringBuffer data = new StringBuffer();

				while (count > 0) {

					boolean escaped = false;
					for (int i = 0; i < count; i++) {
						if (escaped) {
							if (buf[i] == RAW_MODE_CHAR) {
								// report what we have until mode switch:
								ConsoleModelEvent cme = new ConsoleModelEvent(
										PrologSocketConsoleModel.this, data
												.toString(), false);
								fireOutputEvent(cme);
								// skip optional newline
								if (i - 1 < count && buf[i + 1] == '\n') {
									i++;
								}
								// clear output buffer
								data.setLength(0);
								setSingleCharMode(true);
							} else {
								data.append(buf[i]);
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

					try {
						/*
						 * FIXME: UGLY
						 */
						Thread.sleep(1);
					} catch (InterruptedException e) {
						Debug.report(e);
						throw new RuntimeException(e);
					}
					if (!reader.ready() ||
					/*
					 * this is optional: event after eache line; could be
					 * changed to certain character number
					 */
					// data.indexOf("\n") >= 0
							data.length() >= 500) {
						if (data == null) {
							throw new IOException("readLine() returned null");
						}
						if (data.length() > 0) {
							ConsoleModelEvent cme = new ConsoleModelEvent(
									PrologSocketConsoleModel.this, data
											.toString(), false);
							fireOutputEvent(cme);
							data = new StringBuffer();
						}
					} else {
						Debug.warning("READER NOT READY");
					}
					count = reader.read(buf);

				}

			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}

			Debug.info("Console Reader Thread terminating");
		}

	}

	private int port;

	private Thread readerThread = null;

	private boolean singleCharMode = false;

	private String lineBuffer = "";

	private BufferedWriter writer;

	private HashSet listeners = new HashSet();

	private Socket socket;

	private Vector expansions = new Vector();

	private File serverLockFile;

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

	public String getLineBuffer() {
		return lineBuffer;
	}

	public void setLineBuffer(String buffer) {
		String oldBuffer = lineBuffer;
		lineBuffer = buffer;
		fireEditBufferChangedEvent(oldBuffer, lineBuffer);
	}

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
		for (Iterator it = expansions.iterator(); it.hasNext();) {
			QueryExpansion exp = (QueryExpansion) it.next();
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

	public void addConsoleListener(ConsoleModelListener cml) {
		synchronized (listeners) {
			listeners.add(cml);
		}
	}

	public void removeConsoleListener(ConsoleModelListener cml) {
		synchronized (listeners) {
			listeners.remove(cml);
		}
	}

	/**
	 * precondition: we are in single char mode postcondition: we are in line
	 * mode.
	 */
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
		this.singleCharMode = on;
		fireModeChange(new ConsoleModelEvent(this, on));
	}

	public boolean isSingleCharMode() {
		return singleCharMode;
	}

	public synchronized void connect() {
		if (isConnected()) {
			Debug.warning("Seems we are already connected?");
			return;
		}
		long timeout = Long.parseLong(PrologConsolePlugin.getDefault()
				.getPreferenceValue(PDTConsole.PREF_TIMEOUT, "5000"));
		long startTime = System.currentTimeMillis();
		while (!isServerActive()) {
			try {
				Thread.sleep(100);
				long elapsedTime = System.currentTimeMillis() - startTime;
				if (elapsedTime > timeout) {
					throw new RuntimeException(
							"Timeout while waiting for peer to start console service.");
				}
			} catch (InterruptedException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
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
					.getPreferenceValue(PDTConsole.PREF_ENABLE_CONSOLE_VOODOO,
							"false");
			boolean useVoodoo = Boolean.valueOf(valString).booleanValue();
			if (useVoodoo) {
				writer.write("use_module(library(single_char_interceptor)).\n"
						+ "sd_install,"
						+ "set_stream(current_output,tty(true)),"
						+ "set_stream(current_input,tty(true)).\n");
				
			}
			else{
				writer.write("set_stream(current_output,tty(true)),"
						+ "set_stream(current_input,tty(true)).\n");
			}
			writer.flush();
			Debug.debug("Connect complete");
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
	protected void finalize() throws Throwable {
		super.finalize();
		disconnect();
	}

	public synchronized void disconnect() {
		Debug.debug("Disconnect began");
		synchronized (this) {
			try {
				if (socket != null) {
					writer.write("sd_uninstall.\n");
					writer.flush();
					writer.write("end_of_file.\n");
					writer.flush();
					// writer.close();
					readerThread.join(10000);
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
		}

		ConsoleModelEvent cme = new ConsoleModelEvent(this,
				"<<< (Not an) ERROR: Connection to Prolog Process closed >>>",
				true);

		fireOutputEvent(cme);

		Debug.debug("Disconnect complete");
	}

	void fireOutputEvent(ConsoleModelEvent cme) {
		HashSet l;

		synchronized (listeners) {
			l = (HashSet) listeners.clone();
		}

		for (Iterator i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = (ConsoleModelListener) i.next();
			list.onOutput(cme);
		}

	}

	void fireModeChange(ConsoleModelEvent cme) {
		HashSet l;

		synchronized (listeners) {
			l = (HashSet) listeners.clone();
		}

		for (Iterator i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = (ConsoleModelListener) i.next();
			list.onModeChange(cme);
		}

	}

	private void fireCommitEvent(ConsoleModelEvent cme) {
		HashSet l;

		synchronized (listeners) {
			l = (HashSet) listeners.clone();
		}

		for (Iterator i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = (ConsoleModelListener) i.next();
			list.onCommit(cme);
		}
	}

	private void fireEditBufferChangedEvent(String oldBuffer, String buffer) {
		ConsoleModelEvent ev = new ConsoleModelEvent(this, oldBuffer, buffer);

		HashSet l;

		synchronized (listeners) {
			l = (HashSet) listeners.clone();
		}

		for (Iterator i = l.iterator(); i.hasNext();) {
			ConsoleModelListener list = (ConsoleModelListener) i.next();
			list.onEditBufferChanged(ev);
		}
	}

	private boolean isServerActive() {
		return serverLockFile.exists();
	}

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

	public void setLockFile(File lockFile) {
		this.serverLockFile = lockFile;

	}
}
