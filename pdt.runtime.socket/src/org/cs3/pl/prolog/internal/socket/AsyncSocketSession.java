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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import org.cs3.pl.prolog.PrologInterfaceException;
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

	

	private Object lastAbortTicket;

	private HashMap queries= new HashMap();

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
				try{
					while(readAndDispatch());
				} catch(Exception e){
					//there is little we can do here.
					Debug.report(e);
				}
			}

			
		};
		enterBatch();
		
		
	}

	private boolean readAndDispatch() throws PrologInterfaceException {
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
			pif.handleException(e);
		}
		return true;
	}
	
	private void dispatchBatchComplete() {
		fireBatchComplete();
		
		synchronized (tickets) {
			
			
			tickets.clear();
			Vector values = new Vector(tickets.values());
			
			for (Iterator it = values.iterator(); it.hasNext();) {
				Object ticket=it.next();				
				synchronized (ticket) {
					ticket.notifyAll();
				}
			}
			
		}
	}
	
	

	private void dispatchSkippingQuery(int id) {
		Object ticket = getTicket(id);
		fireGoalSkipped(id,ticket);
		removeTicket(id);
		synchronized (ticket) {
			ticket.notifyAll();
		}
	}

	

	private void dispatchJoinComplete(int id) {
		Object ticket = getTicket(id);
		fireJoinComplete(id,ticket);
		removeTicket(id);
		synchronized (ticket) {
			ticket.notifyAll();
		}
	}

	
	private void dispatchAbortComplete(int id) {
		Debug.info("abort complete recieved, id="+id);
		Object ticket = getTicket(id);
		synchronized (lastAbortTicket) {
			if(lastAbortTicket==ticket){
				lastAbortTicket=null;
			}
		}
		removeTicket(id);
		fireAbortComplete(id,ticket);
		Debug.info("listeners were notified about abort completion, id="+id);
		synchronized (ticket) {
			Debug.info("notifying waiting threads, id="+id);
			ticket.notifyAll();
			Debug.info("notifying done, id="+id);
		}
		
	}
	
	
	private void readAndDispatchResults(int id) throws IOException {
		Object ticket = getTicket(id);
		try{				
			while (read_solution(id,ticket));			
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
	
	private void fireJoinComplete(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.joinComplete(e);
			}
		}
		
	}

	private void fireAbortComplete(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.abortComplete(e);
			}
		}
		
		
	}
	
	private void fireGoalSucceeded(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.id=id;
		e.query=getQuery(id);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalSucceeded(e);
			}
		}
	}

	private void fireGoalFailed(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalFailed(e);
			}
		}
		
	}

	private void fireGoalRaisedException(int id,Object ticket, String string) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket,string);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalRaisedException(e);
			}
		}
		
	}

	private void fireGoalHasSolution(int id,Object ticket, Map result) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket,result);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalHasSolution(e);
			}
		}
	}

	private void fireGoalSkipped(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalSkipped(e);
			}
		}
		
	}

	private void fireGoalCut(int id,Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this,ticket);
		e.query=getQuery(id);
		e.id=id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				l.goalCut(e);
			}
		}
		
	}

	
	private boolean read_solution(int id,Object ticket) throws IOException {
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
					fireGoalRaisedException(id,ticket, line.substring(SocketClient.ERROR.length()));
					removeTicket(id);
					return false;
				}
				if (SocketClient.END_OF_SOLUTION.equals(line)) {// yes
					fireGoalHasSolution(id,ticket, result);
					return true;
				}
				if (SocketClient.NO.equals(line)) {
					fireGoalFailed(id,ticket);
					removeTicket(id);
					return false;
				}
				if (SocketClient.YES.equals(line)) {
					fireGoalSucceeded(id,ticket);
					removeTicket(id);
					return false;
				}
				if (SocketClient.CUT.equals(line)) {
					fireGoalCut(id,ticket);
					removeTicket(id);
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
	
	private void exitBatch() throws IOException, PrologInterfaceException{
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call exitBatch() from dispatch thread");
		}
		client.lock();
		try{
			client.writeln(SocketClient.EOB);
			
			dispatcher.join(pif.getTimeout());
			if(dispatcher.isAlive()){
				Debug.error("Dispatcher won't die. Trying to abort it.");
				abort();
				dispatcher.join(pif.getTimeout());
				if(dispatcher.isAlive()){
					Debug.error("Thread is still alive. I will not longer wait for it.");					
				}
			}
			synchronized (listeners) {
				listeners.clear();
			}
			client.readUntil(SocketClient.OK);
		} catch (InterruptedException e) {
			Debug.rethrow(e);			
		}
		finally{
			client.unlock();
		}
	}

	

	public void queryOnce(Object ticket, String query) throws PrologInterfaceException {
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		String mode=canonical?"canonical":"default";
		
		if (!query.endsWith(".")) {
			query=query+".";
		} 
		int id = storeTicket(ticket,query);
		try {
			String command = "query_once("+id+","+mode+").";
			client.writeln(command);
			client.writeln(query);
		} catch (IOException e) {
			pif.handleException(e);
		}
	}
	
	public void queryAll(Object ticket, String query) throws PrologInterfaceException {
		if(isDisposed()){
			throw new IllegalStateException("Session is disposed!");
		}
		String mode=canonical?"canonical":"default";
		if (!query.endsWith(".")) {
			query=query+".";
		} 
		int id = storeTicket(ticket,query);
		try {
			client.writeln("query_all("+id+","+mode+").");
			client.writeln(query);
		} catch (IOException e) {
			pif.handleException(e);
		}
	}

	private int storeTicket(Object ticket,String query) {
		int id=ticketCounter++;
		if(query==null&&!(ticket instanceof _AbortTicket)){
			Debug.debug("debug");
		}
		synchronized (tickets) {
			Integer key = new Integer(id);
			tickets.put(key,ticket);
			queries.put(key,query);
		}		
		return id;
	}

	public boolean isPending(Object ticket){
		synchronized (tickets) {
			return tickets.containsValue(ticket);

		}
	}
	public String getQuery(int id){
		synchronized (tickets) {
			return (String) queries.get(new Integer(id));
		}
	}
	
	
	private Object getTicket(int id){
		synchronized (tickets) {
			Integer key = new Integer(id);			
			Object ticket = tickets.get(key);			
			return ticket;
		}
	}
	
	private void removeTicket(int id){
		synchronized (tickets) {
			Integer key = new Integer(id);			
			tickets.remove(key);			
			queries.remove(key);
		}
	}
	
	
	

	public void join() throws PrologInterfaceException{
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call join() from dispatch thread!");
		}
		Object ticket = new Object();
		int id = storeTicket(ticket,null);
		try {
			synchronized (ticket) {
				if(!disposing){
					client.writeln("join("+id+").");
				}
				while(isPending(ticket)){
					ticket.wait();
				}
			}
			
		} catch (IOException e) {
			pif.handleException(e);
		} catch (InterruptedException e) {
			Debug.rethrow(e);
		}
		
	}
	private static class _AbortTicket{}
	
	public void abort() throws PrologInterfaceException {
		abort(new _AbortTicket());
	}
	public void abort(Object ticket) throws PrologInterfaceException {
		if(ticket==null){
			throw new IllegalArgumentException("null ticket!");
		}
		if(Thread.currentThread()==dispatcher){
			throw new IllegalThreadStateException("Cannot call abort() from dispatch thread!");
		}
		if(client==null){
			return;
		}
		if(isIdle()){
			fireAbortComplete(-1,ticket);
			synchronized (ticket) {
				
				ticket.notifyAll();
			}
			return;
		}
		//Object ticket = new Object();
		lastAbortTicket=ticket;
		int id = storeTicket(ticket,null);
		Debug.info("abort ticket stored, id="+id);
		PrologSession session = pif.getSession();
		try {
			session.queryOnce("thread_send_message('"+client.getProcessorThread()+"', batch_message(abort("+id+")))");
			Debug.info("async abort request queued, id="+id);
			synchronized (ticket) {
				if(!disposing){
					client.writeln("abort("+id+").");			
					Debug.info("sync abort marker queued, id="+id);
				}
				
				while(isPending(ticket)){
					Debug.info("waiting for pending ticket, id="+id);
					if(id==8){
						Debug.debug("debug");
					}
					ticket.wait();
				}
				Debug.info("abort ticket is not/no longer pending, id="+id);
				
			}
			
		} catch (IOException e) {
			pif.handleException(e);
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
			try {
				pif.handleException(e);
			} catch (PrologInterfaceException e1) {
				;
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		} finally {
			client.unlock();
			client = null;
			disposing=false;
		}
	}

	
	public Object getLastAbortTicket(){
		return lastAbortTicket;
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

	public boolean isIdle() {
		synchronized (tickets) {
			return tickets.isEmpty();

		}
	}

	

	
}
