/*
 */
package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.internal.ATermFactory;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;

import org.cs3.pl.prolog.PrologSession2;

/**
 */
public class SocketSession implements PrologSession2 {

	private static final String OPT_CANONICAL = "socketsession.canonical";

	// socketsession.canonical

	private SocketClient client;

	private boolean queryActive;

	private String lastQuery;

	private PrologInterface pif;

	private CTermFactory ctermFactory = new ATermFactory();

	public SocketSession(SocketClient client, PrologInterface pif) {
		this.client = client;
		this.pif = pif;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#dispose()
	 */
	public void dispose() {
		if (isDisposed()) {
			return;
		}
		try {
			client.lock();
			client.close();
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		} finally {
			client.unlock();
			client = null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#query(java.lang.String)
	 */
	public Map query(String query) throws PrologException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		if (query.length() == 0) {
			return new HashMap();
		}
		Map solution;
		endQuery();
		client.lock();
		try {
			client.readUntil(SocketClient.GIVE_COMMAND);
			if (canonical) {
				client.writeln(SocketClient.QUERY_CANONICAL);
			} else {
				client.writeln(SocketClient.QUERY);
			}

			client.readUntil(SocketClient.GIVE_TERM);
			query = query.trim();

			if (query.endsWith(".")) {
				this.lastQuery = query;
				client.writeln(query);
			} else {
				this.lastQuery = query + ".";
				client.writeln(query + ".");
			}

			queryActive = true;
			solution = read_solution();

		} catch (IOException e) {
			client.unlock();
			throw new PrologException("got io trouble. last query was: "
					+ lastQuery, e);
		}
		if (solution == null) {
			endQuery();
		}
		return solution;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryAll(java.lang.String)
	 */
	public List queryAll(String query) throws PrologException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}

		endQuery();
		if (query.length() == 0) {
			List l = new Vector();
			l.add(new HashMap());
			return l;
		}

		client.lock();
		try {
			client.readUntil(SocketClient.GIVE_COMMAND);

			if (canonical) {
				client.writeln(SocketClient.QUERY_ALL_CANONICAL);
			} else {
				client.writeln(SocketClient.QUERY_ALL);
			}

			client.readUntil(SocketClient.GIVE_TERM);
			query = query.trim();
			if (query.endsWith(".")) {
				this.lastQuery = query;
				client.writeln(query);
			} else {
				this.lastQuery = query + ".";
				client.writeln(query + ".");
			}
			Vector results = new Vector();
			Map result = read_solution();
			while (result != null) {
				results.add(result);
				result = read_solution();

			}
			return results;
		} catch (IOException e) {
			throw new PrologException("got io problems. last query was: "
					+ lastQuery, e);
		} finally {
			client.unlock();
		}
	}

	

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryOnce(java.lang.String)
	 */
	public Map queryOnce(String query) throws PrologException {
		Map result = query(query);
		endQuery();
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#next()
	 */
	public Map next() throws PrologException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		if (!queryActive) {
			throw new IllegalStateException("No query active.");
		}
		client.lock();
		Map solution = null;
		try {
			client.readUntil(SocketClient.MORE);
			client.writeln(SocketClient.YES);
			solution = read_solution();
			if (solution == null) {
				endQuery();
			}
		} catch (IOException e) {
			throw new PrologException("got io problems. last query was: "
					+ lastQuery, e);
		} finally {
			client.unlock();
		}
		return solution;
	}

	/**
	 * @return
	 * @throws IOException
	 */
	private Map read_solution() throws IOException {
		HashMap result = new HashMap();
		// try to read a variable name
		while (true) {
			String varname = (String) readValue();
			if (varname == null) {
				// there was no respective data
				String line = client.readln();
				// Debug.debug("parsing: "+line);
				if (line == null) {
					throw new PrologException("don't know what to do.");
				}
				if (line.startsWith(SocketClient.ERROR)) {
					throw new PrologException("Peer reported an error:"
							+ line.substring(SocketClient.ERROR.length())
							+ "\n" + "Last query was: " + lastQuery);
				}
				if (SocketClient.END_OF_SOLUTION.equals(line)) {// yes
					return result;
				}
				if (SocketClient.NO.equals(line)) {// no
					// further
					// solutions
					return null;
				}
				if (SocketClient.YES.equals(line)) {// no
					// further
					// solutions
					/*
					 * note: OK is only legal (according to the protocoo) when
					 * preceeded by END_OF_SOLUTION. (
					 */
					return null;
				}
			} else {
				// so we have a variable name.
				// then there should also be a variabe value.
				Object value = readValue();
				if (value == null) {
					throw new PrologException(
							"could not read value for variable " + varname);
				}
				if (canonical) {
					try {
						value = ctermFactory.createCTerm(value);
					} catch (Throwable e) {

						String msg = "could not parse to cterm: " + value;
						Debug.warning(msg);
						Debug.report(e);
						throw new PrologException(msg,e);
					}
				}
				result.put(varname, value);
			}
		}
	}

	private Object readValue() throws IOException {
		client.lock();
		Object value = null;
		try {
			BufferedReader r = client.getReader();
			// skip whitespace
			r.mark(1);
			int c = r.read();
			while (c != -1 && Character.isWhitespace((char) c)) {
				r.mark(1);
				c = r.read();
			}
			if (c == -1) {
				throw new IOException(
						"read EOF, while skipping whitespace before value.");
			}
			StringBuffer sb = new StringBuffer();
			Stack stack = new Stack();
			// first non-whitespace char should be a '<'
			// otherwise we put reset the stream to the old position and
			// return null

			if (c != '<' && c != '{') {
				r.reset();
				return null;
			}
			while (c != -1) {

				switch (c) {
				case '<':

					// clear buffer
					sb.setLength(0);
					break;
				case '{':
					// push new container
					stack.push(new Vector());
					break;
				case '>':
					// flush and unescape buffer
					value = Util.unescape(sb, 0, sb.length());
					// if the stack is empty, return the value.
					// otherwise, the value is elem of the list lying on top of
					// the stack.
					if (stack.isEmpty()) {
						return value;
					} else {
						List l = (List) stack.peek();
						l.add(value);
					}
					break;
				case '}':
					// if the stack is empty at this point, we have a problem
					if (stack.isEmpty()) {
						throw new PrologException(
								"Read a closing curly bracket (']') but there is no containing list!");
					}
					// pop container from stack.
					value = stack.pop();
					if (stack.isEmpty()) {
						return value;
					} else {
						List l = (List) stack.peek();
						l.add(value);
					}
					break;
				default:
					// append to buffer
					sb.append((char) c);
				}
				c = r.read();
			}
			if (c == -1) {
				throw new IOException(
						"read EOF, while skipping whitespace before value.");
			}

		} finally {
			client.unlock();
		}
		return value;
	}

	/**
	 * @return
	 * @throws IOException
	 * @throws PrologException
	 */
	private Hashtable read_solution_old() throws PrologException, IOException {
		client.lock();
		Hashtable result = new Hashtable();
		try {
			while (true) {

				String line = client.readln();
				// Debug.debug("parsing: "+line);
				if (line == null) {
					throw new PrologException("don't know what to do.");
				}
				if (line.startsWith(SocketClient.ERROR)) {
					throw new PrologException("Peer reported an error:"
							+ line.substring(SocketClient.ERROR.length())
							+ "\n" + "Last query was: " + lastQuery);
				}
				if (SocketClient.END_OF_SOLUTION.equals(line)) {// yes
					return result;
				}
				if (SocketClient.NO.equals(line)) {// no
					// further
					// solutions
					return null;
				}
				if (SocketClient.OK.equals(line)) {// no
					// further
					// solutions
					return null;
				}
				if (!(line.charAt(0) == '<')) {
					throw new RuntimeException("expected '<' at begin of line");
				}

				StringBuffer buf = new StringBuffer(line);
				// make sure we read the complete binding
				Debug.debug("first line: " + line);
				while (!line.endsWith(">")) {
					line = client.readln();
					buf.append("\n" + line);
					Debug.debug("appended: " + line);
				}
				line = buf.toString();

				// read the variable name
				int start = 1;
				int end = line.indexOf('>');
				if (end < start) {
					throw new RuntimeException("ill-formated solution line: "
							+ line);
				}
				String name = Util.unescape(line, start, end);

				// read the variable value
				start = line.indexOf('<', end) + 1;
				if (start < end) {
					throw new RuntimeException("ill-formated solution line: "
							+ line);
				}
				end = line.indexOf('>', start);
				if (end < start) {
					throw new RuntimeException("ill-formated solution line: "
							+ line);
				}
				String value = Util.unescape(line, start, end);
				result.put(name, value);

			}
		} finally {
			client.unlock();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#endQuery()
	 */
	public void endQuery() throws PrologException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		if (!queryActive) {
			return;
		}
		client.lock();
		try {
			while (true) {
				String line = client.readln();
				if (line == null) {
					throw new PrologException("don't know what to do.");
				}
				if (SocketClient.MORE.equals(line)) {
					client.writeln(SocketClient.NO);
				}
				if (SocketClient.OK.equals(line)) {
					return;
				}
				if (line.startsWith(SocketClient.ERROR)) {
					throw new PrologException("Peer reported an error:"
							+ line.substring(SocketClient.ERROR.length()));
				}
			}
		} catch (IOException e) {
			throw new PrologException(e);
		} finally {
			// this is no typo!
			// we need to release lock TWO times:
			// inner lock: in this method.
			// outer lock: in preceeding call to query.
			queryActive = false;
			client.unlock();
			client.unlock();

		}

	}

	private PrologInterfaceListener dispatcher = null;

	private Option[] options;

	private boolean canonical;

	/**
	 * @return Returns the dispatcher.
	 */
	public PrologInterfaceListener getDispatcher() {
		return dispatcher;
	}

	/**
	 * @param dispatcher
	 *            The dispatcher to set.
	 */
	public void setDispatcher(PrologInterfaceListener dispatcher) {
		this.dispatcher = dispatcher;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String)
	 */
	public boolean consult(String name) {
		boolean windowsPlattform = System.getProperty("os.name").indexOf(
				"Windows") > -1;
		if (windowsPlattform) {
			name = name.replace('\\', '/');
		}
		Map r = query("consult('" + name + "')");
		if (r != null && dispatcher != null) {
			dispatcher.update(new PrologInterfaceEvent(this,
					PrologInterface.SUBJECT_CONSULTED, name));
		}
		return r != null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#isDisposed()
	 */
	public boolean isDisposed() {
		// TODO Auto-generated method stub
		return client == null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String,
	 *      java.io.InputStream)
	 */
	public void consult(String name, InputStream content)
			throws PrologException {
		try {
			OutputStream out = new ConsultOutputStream(client, name);
			Util.copy(content, out);
			out.close();
			if (dispatcher != null) {
				dispatcher.update(new PrologInterfaceEvent(this,
						PrologInterface.SUBJECT_CONSULTED, name));
			}
		} catch (IOException e) {
			throw new PrologException(e);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#unconsult(java.lang.String)
	 */
	public void unconsult(String name) throws PrologException {
		try {
			OutputStream out = new ConsultOutputStream(client, name);
			out.close();
			if (dispatcher != null) {
				dispatcher.update(new PrologInterfaceEvent(this,
						PrologInterface.SUBJECT_CONSULTED, name));
			}
		} catch (IOException e) {
			throw new PrologException(e);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#isConsulted(java.lang.String)
	 */
	public boolean isConsulted(String name) throws PrologException {
		return queryOnce("source_file('" + name + "')") != null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#getPrologInterface()
	 */
	public PrologInterface getPrologInterface() {
		return pif;
	}

	public Option[] getOptions() {
		if (options == null) {
			options = new Option[] { new SimpleOption(OPT_CANONICAL,
					"canonical values",
					"if set, the session will answer canonical terms",
					Option.FLAG, "false") };
		}
		return options;
	}

	/**
	 * this implementation does nothing.
	 */
	public void reconfigure() {
		;

	}

	public String getPreferenceValue(String id, String string) {
		if (OPT_CANONICAL.equals(id)) {
			return canonical ? "true" : "false";
		}
		throw new IllegalArgumentException("unkown option id: " + id);
	}

	public void setPreferenceValue(String id, String value) {
		if (OPT_CANONICAL.equals(id)) {
			canonical = Boolean.valueOf(value).booleanValue();
		} else {
			throw new IllegalArgumentException("unkown option id: " + id);
		}
	}

}
