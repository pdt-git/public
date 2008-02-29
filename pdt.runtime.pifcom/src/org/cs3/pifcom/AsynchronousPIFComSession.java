package org.cs3.pifcom;

import java.io.IOException;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pifcom.codec.Message;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.AsyncPrologSessionListener;
import org.cs3.pl.prolog.AsyncPrologSessionListener2;
import org.cs3.pl.prolog.PrologInterfaceException;

public class AsynchronousPIFComSession implements AsyncPrologSession {
	private static final String OPT_CANONICAL = "socketsession.canonical";
	private static final String OPT_INTERPRETE_LISTS = "socketsession.interprete_lists";

	private Option[] options;

	
	
	private boolean canonical;
	private boolean interpreteLists;
	PIFComConnection connection;
	BitSet successfull;
	HashMap<Integer, Object> tickets;
	private HashMap<Integer,String> queries= new HashMap();
	private Vector<AsyncPrologSessionListener> listeners;
	
	private int storeTicket(Object ticket,String query) {
		int id=connection.getNewTicket();
		
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
	

	public void abort() throws PrologInterfaceException {
		abort(new Object());
		

	}

	public void abort(Object monitor) throws PrologInterfaceException {
		int ticket = connection.getNewTicket();
		Message m = Message.abort(ticket);
		connection.sendControlMessage(m);		
		try {
			connection.writeBatchMessage(m);
			do{
				m=connection.readMessage();
			} while(m.getTicket()!=ticket);
		} catch (IOException e) {
			throw new PrologInterfaceException(e);
		}

	}

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
	public void dispose() {
		connection.dispose();		

	}	

	public String getProcessorThreadAlias() {		
		return connection.getThreadAlias();
	}

	public boolean isDisposed() {		
		return connection.isDisposed();
	}

	public boolean isIdle() {
		synchronized (tickets) {
			return tickets.isEmpty();

		}
	}


	
	public void join() throws PrologInterfaceException {
		// TODO Auto-generated method stub

	}

	public void queryAll(Object ticket, String query)
			throws PrologInterfaceException {
		// TODO Auto-generated method stub

	}

	public void queryOnce(Object ticket, String query)
			throws PrologInterfaceException {
		// TODO Auto-generated method stub

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
		} else if (OPT_INTERPRETE_LISTS.equals(id)) {
			interpreteLists = Boolean.valueOf(value).booleanValue();			
		} else {
			throw new IllegalArgumentException("unkown option id: " + id);
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
	private void fireBatchError(Exception e2) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this);
		e.exception=e2;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
				if(l instanceof AsyncPrologSessionListener2){
					((AsyncPrologSessionListener2)l).batchError(e);	
				}
				
			}
		}
		
	}
}
