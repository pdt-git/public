package org.cs3.pifcom;

import java.io.IOException;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pifcom.codec.CTermMessage;
import org.cs3.pifcom.codec.Message;
import org.cs3.pifcom.codec.NameMessage;
import org.cs3.pifcom.codec.UIntMessage;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.AsyncPrologSessionListener;
import org.cs3.pl.prolog.AsyncPrologSessionListener2;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class AsynchronousPIFComSession implements AsyncPrologSession {
		private final class ResultDispatcher extends Thread {
		private int stopTicket = -1;

		public ResultDispatcher(String string) {
			super(string);
		}

		@Override
		public void run() {
			Message m = null;
			try {
				while ((m = connection.readMessage()).getTicket() != stopTicket) {
					dispatchResult(m);
				}
				fireBatchComplete();
			} catch (Exception e) {
				pif.error(e);
			}
		}

		public void stopDispatching() throws Throwable {
			Object monitor = new Object();
			stopTicket = storeTicket(monitor, null,PrologInterface.NONE);
			connection.writeBatchMessage(Message.mark(stopTicket));
			connection.flushBatch();
			join();
		}
	}

	
	PIFComConnection connection;
	BitSet successfull;
	HashMap<Integer, Object> tickets = new HashMap<Integer, Object>();
	private HashMap<Integer, String> queries = new HashMap();
	private Vector<AsyncPrologSessionListener> listeners = new Vector<AsyncPrologSessionListener>();
	private AbstractPrologInterface pif;
	private ResultDispatcher dispatcher;
	private boolean disposed;
	private PrologInterfaceException error;
	private HashMap<Integer, Integer> queryFlags = new HashMap<Integer, Integer>();
	private int flags;

	public AsynchronousPIFComSession(PIFComConnection connection,
			AbstractPrologInterface pif, int flags) {
		this.connection = connection;
		this.pif = pif;
		this.flags=flags;
		dispatcher = new ResultDispatcher("Result Dispatcher "
				+ connection.getThreadAlias());
		dispatcher.start();
	}

	public void dispatchResult(Message m) throws PrologInterfaceException {
		Object ticket = getTicket(m.getTicket());
		int flags = getFlags(m.getTicket());
		switch (m.getOpCode()) {
		case Message.OPC_MARK:
			fireJoinComplete(m.getTicket(), ticket);
			break;
		case Message.OPC_SKIP:
			fireGoalSkipped(m.getTicket(), ticket);
			break;
		case Message.OPC_COMPLETE:
			fireAbortComplete(m.getTicket(), ticket);
			break;
		case Message.OPC_FAIL:
			fireGoalFailed(m.getTicket(), ticket);
			break;
		case Message.OPC_BEGIN_SOLUTION:
			dispatchSolutions(m, ticket,flags);
			break;
		case Message.OPC_ERROR:
			fireGoalRaisedException(m.getTicket(), ticket, getQuery(m
					.getTicket()));
			break;
		case Message.OPC_PROTOCOL_ERROR:
			String s = ((CTermMessage) m).getStringValue();
			fireGoalRaisedException(m.getTicket(), ticket, s);
			break;
		default:
			throw pif.error(new PrologInterfaceException(
					"unexpected message, opc="
							+ Integer.toHexString(m.getOpCode())));
		}
		synchronized (ticket) {
			ticket.notifyAll();
			removeTicket(m.getTicket());
		}
	}

	

	private void dispatchSolutions(Message m, Object ticket,int flags)
			throws PrologInterfaceException {

		int size = ((UIntMessage) m).getIntValue();
		String[] names = new String[size];
		boolean success=false;
		try {
			for (int i = 0; i < size; i++) {
				NameMessage nameMessage = (NameMessage) connection
						.readMessage();
				names[i] = nameMessage.getStringValue();
			}
			Map<String, Object> map = new HashMap<String, Object>();

			int i = 0;
			
			if (size == 0) {
				while ((m = connection.readMessage()).getOpCode() == Message.OPC_EMPTY) {
					fireGoalHasSolution(m.getTicket(), ticket,
							new HashMap<String, Object>());
					success=true;
				}
			} else {
				while ((m = connection.readMessage()).getOpCode() == Message.OPC_BINDING) {
					Object value = PIFComUtils.processBinding((CTermMessage) m,flags);
					map.put(names[i++], value);
					if (i == size) {
						i = 0;
						fireGoalHasSolution(m.getTicket(), ticket, map);
						map = new HashMap<String, Object>();
					}
					success=true;
				}
			}
		} catch (IOException e) {
			throw pif.error(e);
		}

		switch (m.getOpCode()) {
		case Message.OPC_FAIL:
			/* by convention, the asynchronous session reports success after the last solution if there is at least one solution.
			 * also by convention, the pifcom protocol states that solution sets are to be terminated by the report of failure.
			 *  (read: no more solutions available).
			 */
			if(success){
				fireGoalSucceeded(m.getTicket(), ticket);
			}else{
				fireGoalFailed(m.getTicket(),ticket);
			}
			break;
		case Message.OPC_CUT:
			fireGoalCut(m.getTicket(), ticket);
			break;
		case Message.OPC_ERROR:
			fireGoalRaisedException(m.getTicket(), ticket, ((CTermMessage) m)
					.getStringValue());
			break;
		default:
			throw pif.error(new PrologInterfaceException(
					"unexpected message, opc="
							+ Integer.toHexString(m.getOpCode())));

		}

	}

	private int storeTicket(Object ticket, String query,int flags) {
		int id = connection.getNewTicket();

		synchronized (tickets) {
			Integer key = new Integer(id);
			tickets.put(key, ticket);
			queries.put(key, query);
			queryFlags.put(key, flags);
		}
		return id;
	}

	public boolean isPending(Object ticket) {
		synchronized (tickets) {
			return tickets.containsValue(ticket);

		}
	}

	public String getQuery(int id) {
		synchronized (tickets) {
			return (String) queries.get(new Integer(id));
		}
	}

	private Object getTicket(int id) {
		synchronized (tickets) {
			Integer key = new Integer(id);
			Object ticket = tickets.get(key);
			return ticket;
		}
	}
	
	private int getFlags(int id) {
		synchronized(tickets){
			return queryFlags.get(id);
		}
		
	}

	private void removeTicket(int id) {
		synchronized (tickets) {
			Integer key = new Integer(id);
			tickets.remove(key);
			queries.remove(key);
			queryFlags.remove(key);
		}
	}

	public void abort() throws PrologInterfaceException {
		abort(new Object());

	}

	public void abort(Object monitor) throws PrologInterfaceException {
		int ticket = storeTicket(monitor, null,PrologInterface.NONE);
		Message m = Message.abort(ticket);

		try {
			connection.sendControlMessage(m);
			connection.writeBatchMessage(m);
			connection.flushBatch();

		} catch (IOException e) {
			throw pif.error(e);
		}
		waitFor(monitor);
	}

	public void addBatchListener(AsyncPrologSessionListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}

		}

	}

	public void removeBatchListener(AsyncPrologSessionListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}

		}

	}

	public void dispose() {
		if (isDisposed()) {
			return;
		}
		disposed = true;
		error = pif.getError();
		if (error == null) {
			try {
				dispatcher.stopDispatching();
			} catch (Throwable t) {
				pif.error(t);
			}
		}

		connection.dispose();
		connection = null;
	}

	public String getProcessorThreadAlias() {
		return connection.getThreadAlias();
	}

	public boolean isDisposed() {
		return disposed;
	}

	public boolean isIdle() {
		synchronized (tickets) {
			return tickets.isEmpty();

		}
	}

	public void join() throws PrologInterfaceException {
		Object monitor = new Object();
		int ticket = storeTicket(monitor, null,PrologInterface.NONE);
		try {
			connection.writeBatchMessage(Message.mark(ticket));
			connection.flushBatch();

		} catch (IOException e) {
			throw pif.error(e);
		}
		waitFor(monitor);
	}

	protected void waitFor(Object monitor) throws PrologInterfaceException {
		while (isPending(monitor) && !isDisposed()) {
			synchronized (monitor) {
				try {
					monitor.wait(100);

				} catch (InterruptedException e) {
					;
				}

			}
		}
		if (error != null) {
			throw (error);
		}
		if (isDisposed()) {
			throw new PrologInterfaceException(
					"Session was disposed while waiting for monitor: "
							+ monitor);
		}
	}

	public void queryOnce(Object ticket, String query) throws PrologException,
	PrologInterfaceException {
		queryOnce(ticket,query,this.flags);
	}
	
	public void queryOnce(Object ticket, String query,int flags) throws PrologException,
			PrologInterfaceException {
		PLUtil.checkFlags(flags);
		if (query.endsWith(".")) {
			query = query.substring(0, query.length() - 1);
		}
		query = "once((" + query + ")).";
		queryAll(ticket, query,flags);

	}

	public void queryAll(Object ticket, String query) throws PrologException,
	PrologInterfaceException {
		queryAll(ticket,query,this.flags);
	}
	
	public void queryAll(Object ticket, String query,int flags) throws PrologException,
			PrologInterfaceException {
		PLUtil.checkFlags(flags);
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed.");
		}
		if (!query.endsWith(".")) {
			query = query + ".";
		}
		Vector<Map> result = new Vector<Map>();
		int ticketNum = storeTicket(ticket, query,flags);

		try {
			connection.writeBatchMessage(Message.query(ticketNum, query));
			connection.flushBatch();
		} catch (IOException e) {
			throw pif.error(e);
		}
	}

	private void fireBatchComplete() {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.batchComplete(e);
			}
		}

	}

	private void fireJoinComplete(int id, Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.joinComplete(e);
			}
		}

	}

	private void fireAbortComplete(int id, Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.abortComplete(e);
			}
		}

	}

	private void fireGoalSucceeded(int id, Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket);
		e.id = id;
		e.query = getQuery(id);
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.goalSucceeded(e);
			}
		}
	}

	private void fireGoalFailed(int id, Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket);
		e.query = getQuery(id);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.goalFailed(e);
			}
		}

	}

	private void fireGoalRaisedException(int id, Object ticket, String string) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket,
				string);
		e.query = getQuery(id);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.goalRaisedException(e);
			}
		}

	}

	private void fireGoalHasSolution(int id, Object ticket, Map result) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket,
				result);
		e.query = getQuery(id);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.goalHasSolution(e);
			}
		}
	}

	private void fireGoalSkipped(int id, Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket);
		e.query = getQuery(id);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.goalSkipped(e);
			}
		}

	}

	private void fireGoalCut(int id, Object ticket) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this, ticket);
		e.query = getQuery(id);
		e.id = id;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				l.goalCut(e);
			}
		}

	}

	private void fireBatchError(Exception e2) {
		AsyncPrologSessionEvent e = new AsyncPrologSessionEvent(this);
		e.exception = e2;
		synchronized (listeners) {
			for (Iterator it = listeners.iterator(); it.hasNext();) {
				AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
						.next();
				if (l instanceof AsyncPrologSessionListener2) {
					((AsyncPrologSessionListener2) l).batchError(e);
				}

			}
		}

	}

	
}
