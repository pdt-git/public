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

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.internal.ATermFactory;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

/**
 */
public class SocketSession implements PrologSession {

	private SocketClient client;

	private boolean queryActive;

	private String lastQuery;

	private AbstractPrologInterface pif;

	private CTermFactory ctermFactory = new ATermFactory();

	private int flags;

	public SocketSession(SocketClient client, AbstractPrologInterface pif,int flags) {
		this.client = client;
		this.pif = pif;
		this.flags=flags;
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
			if (client != null) {
				client.unlock();
				client = null;
			}

		}
	}

	public List queryAll(String query) throws PrologException,
	PrologInterfaceException {
		return queryAll(query,this.flags);
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryAll(java.lang.String)
	 */
	public List queryAll(String query, int flags) throws PrologException,
			PrologInterfaceException {
		PLUtil.checkFlags(flags);
		
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
			configureProtocol(flags);
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
			Map result = read_solution(flags);
			while (result != null) {
				results.add(result);
				result = read_solution(flags);

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

	public Map queryOnce(String query) throws PrologException,
	PrologInterfaceException {
		return queryOnce(query,this.flags);
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryOnce(java.lang.String)
	 */
	public Map queryOnce(String query, int flags) throws PrologException,
			PrologInterfaceException {
		PLUtil.checkFlags(flags);
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		Map solution;
		if (query.length() == 0) {
			solution = new HashMap();
		} else {

			endQuery();
			client.lock();
			try {
				configureProtocol(flags);
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
				solution = read_solution(flags);

			} catch (IOException e) {
				client.unlock();
				solution = null;
				throw pif.error(e);

			}
			if (solution == null) {
				endQuery();
			}
		}
		
		endQuery();
		return solution;
	}

	

	/**
	 * @return
	 * @throws IOException
	 */
	private Map read_solution(int flags) throws IOException {
		HashMap result = new HashMap();
		// try to read a variable name
		while (true) {			
			String varname = (String) readValue(PrologInterface.UNQUOTE_ATOMS);
			if (varname == null) {
				// there was no respective data
				String line = client.readln();
				// Debug.debug("parsing: "+line);
				if (line == null) {
					throw new IOException("don't know what to do.");
				}
				if (line.startsWith(SocketClient.ERROR)) {
					lastError = new PrologException(line
							.substring(SocketClient.ERROR.length()));
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
				Object value = readValue(flags);
				if (value == null) {
					throw new PrologException(
							"could not read value for variable " + varname);
				}
				
				
				result.put(varname, value);
			}
		}
	}

	private Object readValue(int flags) throws IOException {
		return client.readValue(flags,ctermFactory);
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
					throw new PrologException(line.substring(SocketClient.ERROR
							.length()));
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

	private void configureProtocol(int flags) {
		/*
		 * the only thing that needs to be configured on the server side, is
		 * whether or not lists are processed.
		 */
		boolean processLists = (flags & PrologInterface.PROCESS_LISTS) > 0;
		setProtocolOption("interprete_lists", Boolean.toString(processLists));
	}

	/*
	 * public void setPreferenceValue(String id, String value) {
	 * 
	 * if (OPT_CANONICAL.equals(id)) { canonical =
	 * Boolean.valueOf(value).booleanValue(); setProtocolOption("canonical",
	 * value); } else if (OPT_INTERPRETE_LISTS.equals(id)) { interpreteLists =
	 * Boolean.valueOf(value).booleanValue();
	 * setProtocolOption("interprete_lists", value); } else { throw new
	 * IllegalArgumentException("unkown option id: " + id); } }
	 */
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

	public String getProcessorThreadAlias() throws PrologInterfaceException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		return client.getProcessorThread();
	}

}
