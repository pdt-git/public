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
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;

import org.cs3.pl.prolog.PrologSession2;

/**
 */
public class SocketSession implements PrologSession2 {

	private static final String OPT_CANONICAL = "socketsession.canonical";

	private static final String OPT_INTERPRETE_LISTS = "socketsession.interprete_lists";

	// socketsession.canonical

	private SocketClient client;

	private boolean queryActive;

	private String lastQuery;

	private SocketPrologInterface pif;

	private CTermFactory ctermFactory = new ATermFactory();

	public SocketSession(SocketClient client, SocketPrologInterface pif) {
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
			pif.error(e);
		} finally {
			if(client!=null){
				client.unlock();
				client = null;
			}
			
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#query(java.lang.String)
	 */
	public Map query(String query) throws PrologException,
			PrologInterfaceException {
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

			client.writeln(SocketClient.QUERY);

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
			solution = null;
			throw pif.error(e);

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
	public List queryAll(String query) throws PrologException,
			PrologInterfaceException {
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

			client.writeln(SocketClient.QUERY_ALL);

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
			throw pif.error(e);
			
		} finally {
			if (client != null) {
				client.unlock();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryOnce(java.lang.String)
	 */
	public Map queryOnce(String query) throws PrologException,
			PrologInterfaceException {
		Map result = query(query);
		endQuery();
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#next()
	 */
	public Map next() throws PrologException, PrologInterfaceException {
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
			throw pif.error(e);
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
					throw new IOException("don't know what to do.");
				}
				if (line.startsWith(SocketClient.ERROR)) {
					lastError = new PrologException(line.substring(SocketClient.ERROR.length()));
					throw lastError;
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
						throw new PrologException(msg, e);
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
					value = Util.unescape(sb.toString(), 0, sb.length());
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
						throw new IOException(
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#endQuery()
	 */
	public void endQuery() throws PrologException, PrologInterfaceException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		if (!queryActive) {
			return;
		}
		if (lastError != null) {
			lastError = null;
			queryActive = false;
			return;
		}
		client.lock();
		try {
			while (true) {
				String line = client.readln();
				if (line == null) {
					throw pif.error(new IllegalStateException(
					"don't know what to do."));
				}
				if (SocketClient.MORE.equals(line)) {
					client.writeln(SocketClient.NO);
				}
				if (SocketClient.OK.equals(line)) {
					return;
				}
				if (line.startsWith(SocketClient.ERROR)) {
					throw new PrologException(line.substring(SocketClient.ERROR.length()));
				}
			}
		} catch (IOException e) {
			throw pif.error(e);
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

	private PrologException lastError;

	private boolean interpreteLists;

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
	 * @see org.cs3.pl.prolog.PrologSession#isDisposed()
	 */
	public boolean isDisposed() {
		// TODO Auto-generated method stub
		return client == null;
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
			options = new Option[] {
					new SimpleOption(OPT_CANONICAL, "canonical values",
							"if set, the session will answer canonical terms",
							Option.FLAG, "false"),
					new SimpleOption(OPT_INTERPRETE_LISTS, "interprete lists",
							"if set, the session will use (nested) java.util.List instances to represent"
									+ " prolog list terms.", Option.FLAG,
							"true") };
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
		}else if(OPT_INTERPRETE_LISTS.equals(id)){
			return interpreteLists ? "true" :"false";
		}
		throw new IllegalArgumentException("unkown option id: " + id);
	}

	public void setPreferenceValue(String id, String value) {

		if (OPT_CANONICAL.equals(id)) {
			canonical = Boolean.valueOf(value).booleanValue();
			setProtocolOption("canonical", value);
		} else if (OPT_INTERPRETE_LISTS.equals(id)) {
			interpreteLists = Boolean.valueOf(value).booleanValue();
			setProtocolOption("interprete_lists", value);
		} else {
			throw new IllegalArgumentException("unkown option id: " + id);
		}
	}

	public SocketClient getClient() {
		return client;
	}

	public void setProtocolOption(String id, String value) {
		if (queryActive) {
			throw new RuntimeException(
					"Cannot set protocol option while query is active.");
		}
		client.lock();
		try {
			client.readUntil(SocketClient.GIVE_COMMAND);
			client.writeln(SocketClient.SET_OPTION);
			client.readUntil(SocketClient.GIVE_SYMBOL);
			client.writeln(id);
			client.readUntil(SocketClient.GIVE_TERM);
			client.writeln(value);
			client.readUntil(SocketClient.OK);
		} catch (IOException e) {
			throw new RuntimeException("IO Error while setting protocol option");
		} finally {
			client.unlock();
		}
	}

	@Override
	public String getProcessorThreadAlias() throws PrologInterfaceException {
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		return client.getProcessorThread();
	}

}
