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

package org.cs3.pl.prolog.internal.socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
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
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.AsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

public class AsyncSocketSession implements AsyncPrologSession {
	private static final String OPT_CANONICAL = "socketsession.canonical";

	private Option[] options;

	
	
	private boolean canonical;
	private SocketPrologInterface pif;
	private SocketClient client;

	private Thread dispatcher;

	private boolean disposing;

	private int ticketCounter;

	private HashMap tickets = new HashMap();

	private CTermFactory ctermFactory;

	private Vector listeners=new Vector();

	public void addBatchListener(AsyncPrologSessionListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}

		}
		
	}

	public void removeBatchListener(AsyncPrologSessionListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}

		}
		
	}
	
	public AsyncSocketSession(SocketClient client, SocketPrologInterface pif) throws IOException {
		this.client = client;
		this.pif = pif;	
		ctermFactory=new ATermFactory();
		this.dispatcher = new Thread("Async Query Result Dispatcher"){
			public void run() {
				while(readAndDispatch());
			}

			
		};
		enterBatch();
		
		
	}

	private boolean readAndDispatch() {
		try {
			String line=client.readln().trim();
			if(SocketClient.EOB_COMPLETE.equals(line)){
				dispatchBatchComplete();
				return false;
			}
			else if(line.startsWith(SocketClient.ABORT_COMPLETE)){				
				dispatchAbortComplete(Integer.parseInt(line.substring(SocketClient.ABORT_COMPLETE.length())));
			}
			else if(line.startsWith(SocketClient.JOIN_COMPLETE)){				
				dispatchJoinComplete(Integer.parseInt(line.substring(SocketClient.JOIN_COMPLETE.length())));
			}
			else if(line.startsWith(SocketClient.RESULTS_FOR_QUERY)){				
				readAndDispatchResults(Integer.parseInt(line.substring(SocketClient.RESULTS_FOR_QUERY.length())));
			}
			else if(line.startsWith(SocketClient.SKIPPING_QUERY)){				
				dispatchSkippingQuery(Integer.parseInt(line.substring(SocketClient.SKIPPING_QUERY.length())));
			}
			
		} catch (IOException e) {
			Debug.rethrow(e);
		}
		return true;
	}
	
	private void dispatchBatchComplete() {
		fireBatchComplete();
		synchronized (tickets) {
			for (Iterator it = tickets.values().iterator(); it.hasNext();) {
				Object ticket=it.next();
				synchronized (ticket) {
					ticket.notifyAll();
				}
			}
			tickets.clear();
		}
	}
	
	

	private void dispatchSkippingQuery(int id) {
		Object ticket = findAndRemoveTicket(id);
		fireGoalSkipped(ticket);
		synchronized (ticket) {
			ticket.notifyAll();
		}
	}

	

	private void dispatchJoinComplete(int id) {
		Object ticket = findAndRemoveTicket(id);
		fireJoinComplete(ticket);
		synchronized (ticket) {
			ticket.notifyAll();
		}
	}

	
	private void dispatchAbortComplete(int id) {
		Object ticket = findAndRemoveTicket(id);
		fireAbortComplete(ticket);
		synchronized (ticket) {
			ticket.notifyAll();
		}
		
	}
	
	
	private void readAndDispatchResults(int id) throws IOException {
		Object ticket = findAndRemoveTicket(id);
		try{				
			while (read_solution(ticket));			
		}
		finally{
			synchronized (ticket) {
				ticket.notifyAll();
			}
		}
	}

	private void fireBatchComplete() {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.batchComplete(e);
			}
		}
		
	}
	
	private void fireJoinComplete(Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.joinComplete(e);
			}
		}
		
	}

	private void fireAbortComplete(Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.abortComplete(e);
			}
		}
		
		
	}
	
	private void fireGoalSucceeded(Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalSucceeded(e);
			}
		}
	}

	private void fireGoalFailed(Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalFailed(e);
			}
		}
		
	}

	private void fireGoalRaisedException(Object ticket, String string) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket,string);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalRaisedException(e);
			}
		}
		
	}

	private void fireGoalHasSolution(Object ticket, Map result) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket,result);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalHasSolution(e);
			}
		}
	}

	private void fireGoalSkipped(Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalSkipped(e);
			}
		}
		
	}

	private void fireGoalCut(Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalCut(e);
			}
		}
		
	}

	
	private boolean read_solution(Object ticket) throws IOException {
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
					fireGoalRaisedException(ticket, line.substring(SocketClient.ERROR.length()));
					return false;
				}
				if (SocketClient.END_OF_SOLUTION.equals(line)) {// yes
					fireGoalHasSolution(ticket, result);
					return true;
				}
				if (SocketClient.NO.equals(line)) {
					fireGoalFailed(ticket);
					return false;
				}
				if (SocketClient.YES.equals(line)) {
					fireGoalSucceeded(ticket);
					return false;
				}
				if (SocketClient.CUT.equals(line)) {
					fireGoalCut(ticket);
					return false;
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
	
	

	private void enterBatch() throws IOException {

		client.lock();
		try {
			client.readUntil(SocketClient.GIVE_COMMAND);
			client.writeln(SocketClient.ENTER_BATCH);
			client.readUntil(SocketClient.GO_AHEAD);
		}finally{
			client.unlock();
		}
		dispatcher.start();	
		
	}
	
	private void exitBatch() throws IOException{
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call exitBatch() from dispatch thread");
		}
		client.lock();
		try{
			client.writeln(SocketClient.EOB);
			
			dispatcher.join();
			client.readUntil(SocketClient.OK);
		} catch (InterruptedException e) {
			Debug.rethrow(e);			
		}
		finally{
			client.unlock();
		}
	}

	

	public void queryOnce(Object ticket, String query) {
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		String mode=canonical?"canonical":"default";
		
		if (!query.endsWith(".")) {
			query=query+".";
		} 
		int id = storeTicket(ticket);
		try {
			String command = "query_once("+id+","+mode+").";
			client.writeln(command);
			client.writeln(query);
		} catch (IOException e) {
			Debug.rethrow(e);
		}
	}
	
	public void queryAll(Object ticket, String query) {
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		String mode=canonical?"canonical":"default";
		if (!query.endsWith(".")) {
			query=query+".";
		} 
		int id = storeTicket(ticket);
		try {
			client.writeln("query_all("+id+","+mode+").");
			client.writeln(query);
		} catch (IOException e) {
			Debug.rethrow(e);
		}
	}

	private int storeTicket(Object ticket) {
		int id=ticketCounter++;
		synchronized (tickets) {
			tickets.put(new Integer(id),ticket);
		}		
		return id;
	}

	private Object findAndRemoveTicket(int id){
		synchronized (tickets) {
			return tickets.remove(new Integer(id));
		}
	}
	

	public void join() {
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call join() from dispatch thread!");
		}
		Object ticket = new Object();
		int id = storeTicket(ticket);
		try {
			synchronized (ticket) {
				if(!disposing){
					client.writeln("join("+id+").");
				}
				ticket.wait();
			}
			
		} catch (IOException e) {
			Debug.rethrow(e);
		} catch (InterruptedException e) {
			Debug.rethrow(e);
		}
		
	}
	public void abort() {
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call abort() from dispatch thread!");
		}
		if(client==null){
			return;
		}
		Object ticket = new Object();
		int id = storeTicket(ticket);
		PrologSession session = pif.getSession();
		try {
			session.queryOnce("thread_send_message('"+client.getProcessorThread()+"', batch_message(abort("+id+")))");
			synchronized (ticket) {
				if(!disposing){
					client.writeln("abort("+id+").");					
				}
				
				ticket.wait();

			}
			
		} catch (IOException e) {
			Debug.rethrow(e);
		} catch (InterruptedException e) {
			Debug.rethrow(e);
		}finally{
			session.dispose();
		}
		
	}

	
	public void dispose() {
		if (isDisposed()) {
			return;
		}
		disposing=true;
		client.lock();
		try {
			exitBatch();
			client.close();
		} catch (IOException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		} finally {
			client.unlock();
			client = null;
			disposing=false;
		}
	}

	public boolean isDisposed() {
		return disposing||client == null;
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

	public String getProcessorThreadAlias() {
		
		return client.getProcessorThread();
	}

	

	
}
